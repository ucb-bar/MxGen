package mxgen

import chisel3._
import chisel3.util._
import mxgen.hardfloat._

class MxFpMul(val config: MxConfig, lut: Boolean, val latency: Int = 0) extends Module {
  require(latency >= 0 && latency <= 2,
    s"MxFpMul: latency must be 0, 1, or 2 (got $latency)")

  val cType        = config.accFormat
  val productFmt   = config.productFormat
  val outBias      = productFmt.bias
  val laneExpWidth = productFmt.exp + 1

  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val type_a = Input(new MxTypeBundle())
    val in_weights = Input(UInt(config.inWeiBusWidth.W))
    val type_w = Input(new MxTypeBundle())
    val mode = Input(new mxMode())
    val enable = Input(Bool())
    val rec_c = Input(UInt((config.numActiveOutputLanes * config.accFormat.recoded).W))
    val out = Output(UInt((config.numActiveOutputLanes * config.accFormat.recoded).W))
  })

  // Mul side lives in MxFpMulCore (shared with MxDotProduct); add side is one
  // of three implementations gated below.
  val core = Module(new MxFpMulCore(config, lut))
  core.io.in_activation := io.in_activation
  core.io.type_a        := io.type_a
  core.io.in_weights    := io.in_weights
  core.io.type_w        := io.type_w
  core.io.mode          := io.mode
  core.io.enable        := io.enable

  val peMagW  = core.io.peMag
  val peExpW  = core.io.peExp
  val peSignW = core.io.peSign
  val peZeroW = core.io.peIsZero
  val peIsNaN = core.io.peIsNaN
  val expSignedW = core.io.expSignedRaw

  // A product must flush to zero when its post-normalization magnitude rounds to zero in the
  // accumulator format (otherwise the exponent subtract wraps and saturates to Inf). Under RNE
  // that boundary is HALF the smallest subnormal: a product in [1/2, 1) x smallest rounds UP to
  // the smallest subnormal (kept), only below 1/2 x smallest rounds to zero. ACC_FLOOR is the
  // smallest subnormal's biased exponent (product-format bias); flush strictly below ACC_FLOOR-1.
  val ACC_FLOOR = productFmt.bias - cType.bias + 2 - cType.sig
  // The product path itself (productFmt, before rounding into the accumulator) can only represent
  // down to its smallest subnormal-with-extended-range, whose biased exponent is
  // 2 - productFmt.sig - productFmt.bias (empirically 2^-16 for the E4M3 (4,4) product format).
  // Below that the exponent field wraps and MxPEOutToRaw reads it as OVERFLOW -> saturates to a
  // huge value (~448). For a NARROW accumulator ACC_FLOOR-1 sits above this, so the RNE flush
  // already fires first. But for a WIDE accumulator -- e.g. (8,8) at mesh row 15 -- ACC_FLOOR drops
  // far below the wrap point (ACC_FLOOR-1 = -127), so a subnormal x subnormal product slips through
  // unflushed and blows up (the fp8 128x128 requant corruption). Clamp the flush threshold so it
  // never drops below the product-path wrap point; the flushed-to-zero value is negligible vs the
  // O(1) running accumulator, so this stays bit-exact against the Spike reference.
  val PROD_FLOOR  = 2 - productFmt.sig - productFmt.bias
  val FLUSH_FLOOR = math.max(ACC_FLOOR - 1, PROD_FLOOR)
  def isUnderflow(expSigned: SInt, nExp: UInt, nDir: Bool): Bool = {
    val trueExp = Mux(!nDir, expSigned - nExp.zext, expSigned + nExp.zext)
    trueExp < FLUSH_FLOOR.S
  }

  // latency=2: one reg before the adder + one reg inside MxPEAddRecFN.
  // fpnew maps `latency` directly to NumPipeRegs.
  val addLatency  = if (latency >= 1) 1 else 0
  val preAddRegs  = if (latency >= 2) 1 else 0
  if (config.useFpnewAdder) {
    require(config.accFormat.exp == 8 && config.accFormat.sig == 8,
      s"useFpnewAdder requires accFormat=BF16(8,8); got ${config.accFormat}")
    require(!config.useMxPEAddRecFN,
      "useFpnewAdder and useMxPEAddRecFN are mutually exclusive")
  }
  val useDefault = !config.useFpnewAdder && !config.useMxPEAddRecFN
  val addUnits: Option[Seq[hardfloatHelper.MxPEAddRecFN]] =
    if (!config.useMxPEAddRecFN) None
    else Some(Seq.fill(config.numActiveOutputLanes)(
      Module(new hardfloatHelper.MxPEAddRecFN(cType.exp, cType.sig, laneExpWidth, outBias, addLatency))))
  val outputs = Wire(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  val recIn_c = io.rec_c.asTypeOf(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  for (i <- 0 until config.numActiveOutputLanes) {
    if (config.useMxPEAddRecFN) {
      def pipe[T <: chisel3.Data](sig: T, n: Int): T =
        (0 until n).foldLeft(sig)((s, _) => RegNext(s))
      val unit = addUnits.get(i)
      unit.io.roundingMode   := hardfloat.consts.round_near_even
      unit.io.detectTininess := hardfloat.consts.tininess_afterRounding
      unit.io.peMag    := pipe(peMagW(i),  preAddRegs)
      unit.io.peExp    := pipe(peExpW(i),  preAddRegs)
      unit.io.peSign   := pipe(peSignW(i), preAddRegs)
      unit.io.peIsZero := pipe(peZeroW(i), preAddRegs)
      unit.io.peIsNaN  := pipe(peIsNaN,    preAddRegs)
      unit.io.c        := pipe(recIn_c(i), preAddRegs)

      outputs(i) := unit.io.out
    }
  }

  // Default add chain (mirrors original gemmini MxFpMul):
  //   normalize(raw_product) → MxPEOutToRaw → RoundAnyRawFNToRecFN(productFmt → accFmt) → MxMulAddRecFN.
  // Validated across narrow accFormats and matches old-gemmini bit behavior.
  if (useDefault) {
    val out_pe = core.io.rawProduct
    val out_e  = peExpW
    val out_signs = (0 until config.numActiveOutputLanes).map(i => peSignW(i))
    val laneOutW = config.outPE_width / 4
    val laneHalfW = config.outPE_width / 2
    val typeA  = io.type_a
    val typeW  = io.type_w

    def normalize(prod: UInt, outBits: Int, inBits: Int): (UInt, UInt, Bool) = {
      if (inBits > outBits) {
        val isZero = prod === 0.U
        val isPositiveShift = prod(inBits-1)
        val leftShift = Mux(isPositiveShift, 0.U, PriorityEncoder(prod.asBools.reverse))
        val expAdj = Mux(isPositiveShift, 1.U, leftShift -& 1.U)
        val aligned = prod << leftShift
        (Mux(isZero, 0.U(outBits.W), aligned(inBits - 2, inBits - 1 - outBits)),
         Mux(isZero, 0.U, expAdj),
         isPositiveShift)
      } else {
        val isZero = prod === 0.U
        val extraPad = outBits - inBits
        val realProd = prod(inBits - 1, 0)
        val isPositiveShift = prod(inBits-1)
        val leftShift = Mux(isPositiveShift, 1.U, PriorityEncoder(realProd.asBools.reverse) +& 1.U)
        val expAdj = Mux(isPositiveShift, 1.U, leftShift - 2.U)
        val aligned = realProd << leftShift
        ((Mux(isZero, 0.U(outBits.W), aligned(inBits-1, 0) << extraPad)(outBits-1, 0)),
         Mux(isZero, 0.U, expAdj),
         isPositiveShift)
      }
    }

    def resize(in: RawFloat, inT: MxFormat, outT: MxFormat): RawFloat = {
      val u = Module(new RoundAnyRawFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig, 0))
      u.io.in := in
      u.io.roundingMode := hardfloat.consts.round_near_even
      u.io.detectTininess := hardfloat.consts.tininess_afterRounding
      u.io.invalidExc := false.B
      u.io.infiniteExc := false.B
      rawFloatFromRecFN(outT.exp, outT.sig, u.io.out)
    }

    // Gate per-width normalize() variants on whether any 4-output mode actually
    // uses that product width. Avoids out-of-range slicing on out_pe for narrow
    // configs like fp4Only (laneOutW=4).
    val out4Pairs = config.modesSupported.filter(_.numOutputs == 4)
                          .map(m => (m.actWidth, m.weiWidth)).toSet
    val needSig33 = laneOutW >= 6 && out4Pairs.contains((3, 3))
    val needSigMix = laneOutW >= 5 && (out4Pairs.contains((2, 3)) || out4Pairs.contains((3, 2)))
    val needSig22 = out4Pairs.contains((2, 2))

    val out4_toRec = if (config.needsOut4) Some(VecInit.tabulate(4) { i =>
      val base = i * laneOutW
      val n33 = if (needSig33)  Some(normalize(out_pe(base + 5, base), productFmt.sig - 1, 6)) else None
      val nmx = if (needSigMix) Some(normalize(out_pe(base + 4, base), productFmt.sig - 1, 5)) else None
      val n22 = if (needSig22)  Some(normalize(out_pe(base + 3, base), productFmt.sig - 1, 4)) else None
      val choices = Seq(n33, nmx, n22).flatten
      require(choices.nonEmpty, "MxFpMul default path: no 4-output product variant configured")
      val fallback = choices.head
      def pick[T](pred: Bool, opt: Option[T], dflt: T): T = if (opt.isDefined) opt.get else dflt
      val rec_exp   = Mux(typeA.sig === 2.U && typeW.sig === 2.U, n22.map(_._2).getOrElse(fallback._2),
                       Mux(typeA.sig === 3.U && typeW.sig === 3.U, n33.map(_._2).getOrElse(fallback._2),
                         nmx.map(_._2).getOrElse(fallback._2)))
      val shift_dir = Mux(typeA.sig === 2.U && typeW.sig === 2.U, n22.map(_._3).getOrElse(fallback._3),
                       Mux(typeA.sig === 3.U && typeW.sig === 3.U, n33.map(_._3).getOrElse(fallback._3),
                         nmx.map(_._3).getOrElse(fallback._3)))
      val rec_sig   = Mux(typeA.sig === 2.U && typeW.sig === 2.U, n22.map(_._1).getOrElse(fallback._1),
                       Mux(typeA.sig === 3.U && typeW.sig === 3.U, n33.map(_._1).getOrElse(fallback._1),
                         nmx.map(_._1).getOrElse(fallback._1)))
      MxPEOutToRaw(productFmt.exp, productFmt.sig, out_signs(i),
        Mux(!shift_dir, out_e(i) -% rec_exp, out_e(i) +% rec_exp),
        rec_sig, peIsNaN, peZeroW(i) || isUnderflow(expSignedW(i), rec_exp, shift_dir))
    }) else None

    val out2Pairs = config.modesSupported.filter(_.numOutputs == 2)
                          .map(m => (m.actWidth, m.weiWidth)).toSet
    val need2Sig44 = laneHalfW >= 7 && out2Pairs.exists { case (a, w) => a >= 3 && w >= 3 }
    val need2Smaller = laneHalfW >= 6

    val out2_toRec = if (config.needsOut2) Some(VecInit.tabulate(2) { i =>
      val base = i * laneHalfW
      val n44 = if (need2Sig44)    Some(normalize(out_pe(base + 6, base), productFmt.sig - 1, 7)) else None
      val nsm = if (need2Smaller)  Some(normalize(out_pe(base + 5, base), productFmt.sig - 1, 6)) else None
      val choices = Seq(n44, nsm).flatten
      require(choices.nonEmpty, "MxFpMul default path: no 2-output product variant configured")
      val fallback = choices.head
      val rec_exp   = Mux(typeA.sig === 2.U || typeW.sig === 2.U, nsm.map(_._2).getOrElse(fallback._2),
                                                                  n44.map(_._2).getOrElse(fallback._2))
      val shift_dir = Mux(typeA.sig === 2.U || typeW.sig === 2.U, nsm.map(_._3).getOrElse(fallback._3),
                                                                  n44.map(_._3).getOrElse(fallback._3))
      val rec_sig   = Mux(typeA.sig === 2.U || typeW.sig === 2.U, nsm.map(_._1).getOrElse(fallback._1),
                                                                  n44.map(_._1).getOrElse(fallback._1))
      MxPEOutToRaw(productFmt.exp, productFmt.sig, out_signs(i * 2),
        Mux(!shift_dir, peExpW(i * 2) -% rec_exp, peExpW(i * 2) +% rec_exp),
        rec_sig, peIsNaN, peZeroW(i * 2) || isUnderflow(expSignedW(i * 2), rec_exp, shift_dir))
    }) else None

    val out1_toRec = if (config.needsOut1) Some({
      val singleOut = config.modesSupported.find(_.numOutputs == 1).get
      val prodW = singleOut.outTotalWidth
      val n = normalize(out_pe(prodW - 1, 0), productFmt.sig - 1, prodW)
      MxPEOutToRaw(productFmt.exp, productFmt.sig, out_signs(0),
        Mux(!n._3, peExpW(0) -% n._2, peExpW(0) +% n._2),
        n._1, peIsNaN, peZeroW(0) || isUnderflow(expSignedW(0), n._2, n._3))
    }) else None

    def pipe[T <: chisel3.Data](sig: T, n: Int): T =
      (0 until n).foldLeft(sig)((s, _) => RegNext(s))

    for (i <- 0 until config.numActiveOutputLanes) {
      val rawIn: RawFloat = (config.needsOut4, config.needsOut2, config.needsOut1) match {
        case (true, true, true) =>
          val raw4 = resize(out4_toRec.get(i), productFmt, cType)
          val raw2 = resize(out2_toRec.get(i / 2), productFmt, cType)
          val raw1 = resize(out1_toRec.get, productFmt, cType)
          val sel = Wire(new RawFloat(cType.exp, cType.sig))
          when (io.mode.numOutputs === 1.U) { sel := raw1 }
            .elsewhen (io.mode.numOutputs === 2.U) { sel := raw2 }
            .otherwise { sel := raw4 }
          sel
        case (true, false, true) =>
          val raw4 = resize(out4_toRec.get(i), productFmt, cType)
          val raw1 = resize(out1_toRec.get, productFmt, cType)
          val sel = Wire(new RawFloat(cType.exp, cType.sig))
          when (io.mode.numOutputs === 1.U) { sel := raw1 } .otherwise { sel := raw4 }
          sel
        case (true, true, false) =>
          val raw4 = resize(out4_toRec.get(i), productFmt, cType)
          val raw2 = resize(out2_toRec.get(i / 2), productFmt, cType)
          val sel = Wire(new RawFloat(cType.exp, cType.sig))
          when (io.mode.numOutputs === 2.U) { sel := raw2 } .otherwise { sel := raw4 }
          sel
        case (false, true, true) =>
          val raw2 = resize(out2_toRec.get(i / 2), productFmt, cType)
          val raw1 = resize(out1_toRec.get, productFmt, cType)
          val sel = Wire(new RawFloat(cType.exp, cType.sig))
          when (io.mode.numOutputs === 1.U) { sel := raw1 } .otherwise { sel := raw2 }
          sel
        case (true, false, false) => resize(out4_toRec.get(i), productFmt, cType)
        case (false, true, false) => resize(out2_toRec.get(i / 2), productFmt, cType)
        case (false, false, true) => resize(out1_toRec.get, productFmt, cType)
        case _ => throw new IllegalStateException("MxFpMul default path: no needsOut*")
      }
      val add = Module(new hardfloatHelper.MxMulAddRecFN(cType.exp, cType.sig))
      add.io.roundingMode := hardfloat.consts.round_near_even
      add.io.detectTininess := hardfloat.consts.tininess_afterRounding
      add.io.a := pipe(rawIn, preAddRegs)
      add.io.c := pipe(recIn_c(i), preAddRegs)
      outputs(i) := (if (addLatency >= 1) RegNext(add.io.out) else add.io.out)
    }
  }

  // fpnew/cvfpu BF16 adder path: round raw product to BF16, then add.
  if (config.useFpnewAdder) {
    val numLanes     = config.numActiveOutputLanes
    val productBias  = config.productFormat.bias
    val peMagWidth   = cType.sig
    val peExpWidth   = laneExpWidth
    val sExpW        = peExpWidth + 2
    val recodedOff   = (BigInt(1) << peExpWidth) - BigInt(productBias)

    def peProductToBf16Ieee(
      peMag: UInt, peExp: UInt, peSign: Bool, peIsZero: Bool, peIsNaN: Bool
    ): UInt = {
      val isZero   = peIsZero || (peMag === 0.U)
      val normDist = countLeadingZeros(peMag)
      val normMag  = (peMag << normDist)(peMagWidth - 1, 0)
      val sExpUInt = peExp +& recodedOff.U(sExpW.W) - normDist
      val sExpSInt = sExpUInt(sExpW - 1, 0).asSInt
      val sig = 0.U(1.W) ## (!isZero) ## normMag(peMagWidth - 2, 0)

      val raw = Wire(new RawFloat(peExpWidth, peMagWidth))
      raw.isNaN  := peIsNaN
      raw.isInf  := false.B
      raw.isZero := isZero
      raw.sign   := peSign
      raw.sExp   := sExpSInt
      raw.sig    := sig

      val cvt = Module(new RoundAnyRawFNToRecFN(peExpWidth, peMagWidth, 8, 8, 0))
      cvt.io.in             := raw
      cvt.io.invalidExc     := false.B
      cvt.io.infiniteExc    := false.B
      cvt.io.roundingMode   := hardfloat.consts.round_near_even
      cvt.io.detectTininess := hardfloat.consts.tininess_afterRounding
      fNFromRecFN(8, 8, cvt.io.out)
    }

    val laneProductIeee = (0 until numLanes).map { i =>
      peProductToBf16Ieee(peMagW(i), peExpW(i), peSignW(i), peZeroW(i), peIsNaN)
    }
    val laneCIeee = (0 until numLanes).map { i =>
      fNFromRecFN(8, 8, recIn_c(i))
    }
    val adder = Module(new mxgen.cvfpu.MxFpnewBf16Add(numLanes = numLanes, numPipeRegs = latency))
    adder.io.clock := clock
    adder.io.reset := reset
    adder.io.a_i   := Cat(laneProductIeee.reverse)
    adder.io.b_i   := Cat(laneCIeee.reverse)

    val outVec = adder.io.out_o.asTypeOf(Vec(numLanes, UInt(16.W)))
    for (i <- 0 until numLanes) {
      outputs(i) := recFNFromFN(8, 8, outVec(i))
    }
  }

  io.out := outputs.asUInt
}

object MxPEOutToRaw {
  // Ported from gemmini inline commit 3fbd243 "fix subnormal handling for narrow
  // acc/prod precisions". The previous version renormalized expIn==0 inputs via
  // CLZ on fractIn, which produced wrong sExp/sig for narrow precisions where the
  // product is already normalized by normalize() upstream.
  def apply(expWidth: Int, sigWidth: Int, sign: UInt, exp: UInt, sig: UInt, inputNaN: Bool, inputZero: Bool): RawFloat = {
    val expIn = exp(expWidth, 0)
    val fractIn = sig(sigWidth-2, 0)

    val adjustedExp = expIn + ((BigInt(1) << (expWidth - 1)).U | 1.U)
    val isSpecial = adjustedExp(expWidth, expWidth - 1) === 3.U

    val satExp  = (BigInt(3) << (expWidth - 1)).U((expWidth + 1).W)
    val satFrac = ((BigInt(1) << (sigWidth - 1)) - 2).U((sigWidth - 1).W)
    val saturate      = isSpecial && !inputNaN
    val effectiveZero = inputZero && !inputNaN

    val out = Wire(new RawFloat(expWidth, sigWidth))
    out.isNaN  := inputNaN
    out.isInf  := false.B
    out.isZero := effectiveZero
    out.sign   := sign
    out.sExp   := Mux(inputNaN || saturate, satExp, adjustedExp(expWidth, 0)).zext
    out.sig    := 0.U(1.W) ## !effectiveZero ## Mux(inputNaN || saturate, satFrac, fractIn)
    out
  }
}

package mxgen

import chisel3._
import chisel3.util._
import mxgen.hardfloat._

class MxFpMul(val config: MxConfig, lut: Boolean, val latency: Int = 0) extends Module {
  require(latency >= 0 && latency <= 2,
    s"MxFpMul: latency must be 0, 1, or 2 (got $latency)")
  println("Creating MxFpMul with product precision: " + config.productFormat + " and acc precision: " + config.accFormat + s" (latency=$latency)")
  println(config.describe)

  val outType1 = config.productFormat
  val outType2 = config.productFormat
  val outType4 = config.productFormat
  val cType = config.accFormat
  val totalAdderWidth = 4*(outType4.exp + 1)

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

  val actType: MxTypeBundle = if (config.needsRuntimeActType) io.type_a else {
    val t = Wire(new MxTypeBundle())
    val f = config.actFormats.head
    t.exp := f.expWidth.U; t.sig := f.sigWidth.U; t
  }
  val weiType: MxTypeBundle = if (config.needsRuntimeWeiType) io.type_w else {
    val t = Wire(new MxTypeBundle())
    val f = config.weiFormats.head
    t.exp := f.expWidth.U; t.sig := f.sigWidth.U; t
  }
  val modeWire: mxMode = if (config.needsRuntimeMode) io.mode else {
    val m = Wire(new mxMode())
    val p = config.modesSupported.head
    m.actWidth   := p.actWidth.U
    m.weiWidth   := p.weiWidth.U
    m.weiInputs  := p.weiInputs.U
    m.actInputs  := p.actInputs.U
    m.shift(0)(0) := p.shift(0)(0).U; m.shift(0)(1) := p.shift(0)(1).U
    m.shift(1)(0) := p.shift(1)(0).U; m.shift(1)(1) := p.shift(1)(1).U
    m.numOutputs := p.numOutputs.U
    m
  }

  def normalize(prod: UInt, outBits: Int, inBits: Int): (UInt, UInt, Bool) = {
    if (inBits > outBits) {
      val isZero = prod === 0.U
      val isPositiveShift = prod(inBits-1)
      val leftShift = Mux(isPositiveShift, 0.U, PriorityEncoder(prod.asBools.reverse))
      val expAdj = Mux(isPositiveShift, 1.U, leftShift -& 1.U)
      val aligned = prod << leftShift

      val truncated = aligned(inBits - 2, inBits - 1 - outBits)

      val bitsBelow = inBits - 1 - outBits
      val roundBit: Bool  = if (bitsBelow >= 1) aligned(inBits - 2 - outBits)       else false.B
      val stickyBit: Bool = if (bitsBelow >= 2) aligned(inBits - 3 - outBits, 0).orR else false.B
      val doRound = roundBit && (stickyBit || truncated(0))

      val rounded       = truncated +& doRound
      val roundOverflow = rounded(outBits)
      val finalSig      = Mux(roundOverflow, 0.U(outBits.W), rounded(outBits - 1, 0))
      val finalExpAdj   = expAdj +& roundOverflow

      (Mux(isZero, 0.U(outBits.W), finalSig), Mux(isZero, 0.U, finalExpAdj), isPositiveShift)
    } else {
      val isZero = prod === 0.U
      val extraPad = outBits - inBits
      val realProd = prod(inBits - 1, 0)
      val isPositiveShift = prod(inBits-1)
      val leftShift = Mux(isPositiveShift, 1.U, PriorityEncoder(realProd.asBools.reverse) +& 1.U)
      val expAdj = Mux(isPositiveShift, 1.U, leftShift - 2.U)
      val aligned = realProd << leftShift
      ((Mux(isZero, 0.U(outBits.W), aligned(inBits-1, 0) << (extraPad))(outBits-1, 0)), Mux(isZero, 0.U, expAdj), isPositiveShift)

      }
    }

  def pack(c : MxClassifiedFp, w_exp: Int, w_sig: Int, padExp: Int): (UInt, UInt, UInt) = {
    val packed_sig = Mux(c.isZero, 0.U, (~c.isSub.asUInt ## c.sig))
    val packed_exp = Mux(c.isZero, 0.S, c.exp.asSInt)
    if (w_exp > padExp) {
      (packed_exp.pad(padExp).asUInt, packed_sig.pad(w_sig), c.sign)
    } else {
      (packed_exp.pad(w_exp).asUInt.pad(padExp), packed_sig.pad(w_sig), c.sign)
    }
  }

  // Shared classify+pack: per-format cones in parallel, Mux1H'd on runtime format.
  // Exp is sign-extended into the slot — MxExp re-reads it as SInt.
  def classifyAndPackSlot(
    lane:         UInt,
    typeBundle:   MxTypeBundle,
    formats:      Seq[MxFormat],
    slotSigWidth: Int,
    slotExpWidth: Int
  ): (UInt, UInt, UInt, Bool, Bool) = {
    require(formats.nonEmpty, "classifyAndPackSlot: formats must be non-empty")

    val sels: Seq[Bool] = formats.map { f =>
      typeBundle.exp === f.expWidth.U && typeBundle.sig === f.sigWidth.U
    }

    val perFmt = formats.map { f =>
      val w      = f.bitWidth
      val sign   = lane(w - 1)
      val rawExp = lane(w - 2, f.sigWidth - 1)
      val rawSig = if (f.sigWidth > 1) lane(f.sigWidth - 2, 0) else 0.U(1.W)
      val nz     = if (f.sigWidth > 1) rawSig.orR else false.B
      val isZero = (rawExp === 0.U) && !nz
      val isSub  = (rawExp === 0.U) && nz
      val isNaN  = if (w == 8) rawExp.andR && nz else false.B
      // Format-native UInt modular subtractor matching classify()'s output
      // c.exp. Width is f.expWidth; callers sign-extend to the slot width.
      val adjExp: UInt =
        (Mux(isSub, 1.U(f.expWidth.W), rawExp) -% f.bias.U(f.expWidth.W)) -% 1.U(1.W)
      (sign, rawSig, isNaN, isZero, isSub, adjExp)
    }

    val muxedSign:  UInt = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._1 })
    val muxedIsNaN: Bool = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._3 })
    val muxedIsZero: Bool = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._4 })
    val muxedIsSub:  Bool = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._5 })

    // Sign-extend each format's adjExp to slotExpWidth (two-s-complement) and
    // reinterpret as UInt — matches `packed_exp.pad(slotExpWidth).asUInt`.
    val muxedAdjExp: UInt = Mux1H(sels.zip(perFmt).map {
      case (sel, t) => sel -> t._6.asSInt.pad(slotExpWidth).asUInt
    })
    val expSlot: UInt = Mux(muxedIsZero, 0.U(slotExpWidth.W), muxedAdjExp)

    val leading = (~muxedIsSub).asUInt
    val sigContent: UInt = Mux1H(sels.zip(perFmt).map {
      case (sel, t) => sel -> Cat(leading, t._2).pad(slotSigWidth)
    })
    val sigSlot: UInt = Mux(muxedIsZero, 0.U(slotSigWidth.W), sigContent)

    (sigSlot, expSlot, muxedSign, muxedIsZero, muxedIsNaN)
  }

  // Separate input into lanes
  val lanes2_a = io.in_activation.asTypeOf(Vec(2, UInt((config.inActBusWidth/2).W)))
  val lanes1_a = io.in_activation.asTypeOf(Vec(1, UInt((config.inActBusWidth).W)))
  val lanes2_w = io.in_weights.asTypeOf(Vec(2, UInt((config.inWeiBusWidth/2).W)))
  val lanes1_w = io.in_weights.asTypeOf(Vec(1, UInt((config.inWeiBusWidth).W)))

  // Wire up elements for PE + Exp Adder
  val inA_pe   = WireDefault(0.U(config.inPE_act_totalWidth.W))
  val inW_pe   = WireDefault(0.U(config.inPE_wei_totalWidth.W))
  val inA_exp  = WireDefault(0.U((config.inActBusWidth - config.inPE_act_totalWidth).W))
  val inW_exp  = WireDefault(0.U((config.inWeiBusWidth - config.inPE_wei_totalWidth).W))
  val inA_sign = WireDefault(0.U(2.W))
  val inW_sign = WireDefault(0.U(2.W))
  val in_a_mask= WireDefault("b11".U(2.W))
  val in_w_mask= WireDefault("b11".U(2.W))
  val nanA = WireDefault(false.B)
  val nanW = WireDefault(false.B)

  // Classify input lanes: dual (sig<4, two elements/pair) vs single (sig>=4,
  // broadcast sign/mask). Top sig bit picks between paths at runtime.
  val dualActFormats   = config.actFormats.filter(_.sigWidth < 4).toSeq.sortBy(_.bitWidth)
  val singleActFormats = config.actFormats.filter(_.sigWidth >= 4).toSeq.sortBy(_.bitWidth)
  val dualWeiFormats   = config.weiFormats.filter(_.sigWidth < 4).toSeq.sortBy(_.bitWidth)
  val singleWeiFormats = config.weiFormats.filter(_.sigWidth >= 4).toSeq.sortBy(_.bitWidth)

  // --- Activation classifier -------------------------------------------------
  {
    val slotSigDual  = config.inPE_act_totalWidth / 2
    val slotExpDual  = (config.inActBusWidth - config.inPE_act_totalWidth) / 2
    val slotSigSng   = config.inPE_act_totalWidth
    val slotExpSng   = config.inActBusWidth - config.inPE_act_totalWidth

    val dualRes = if (dualActFormats.nonEmpty) Some(
      (0 until 2).map { i =>
        classifyAndPackSlot(lanes2_a(i), actType, dualActFormats, slotSigDual, slotExpDual)
      }
    ) else None

    val sngRes = if (singleActFormats.nonEmpty) Some(
      classifyAndPackSlot(lanes1_a(0), actType, singleActFormats, slotSigSng, slotExpSng)
    ) else None

    val dualSig  = dualRes.map(r => Cat(r(1)._1, r(0)._1))
    val dualExp  = dualRes.map(r => Cat(r(1)._2, r(0)._2))
    val dualSign = dualRes.map(r => Cat(r(1)._3, r(0)._3))
    val dualMask = dualRes.map(r => Cat(r(1)._4.asUInt, r(0)._4.asUInt))
    val dualNaN  = dualRes.map(r => r(0)._5 || r(1)._5)

    val sngSig   = sngRes.map(_._1)
    val sngExp   = sngRes.map(_._2)
    val sngSign  = sngRes.map(r => Fill(2, r._3))
    val sngMask  = sngRes.map(r => Fill(2, r._4.asUInt))
    val sngNaN   = sngRes.map(_._5)

    (dualRes, sngRes) match {
      case (Some(_), None) =>
        inA_pe    := dualSig.get
        inA_exp   := dualExp.get
        inA_sign  := dualSign.get
        in_a_mask := dualMask.get
        nanA      := dualNaN.get
      case (None, Some(_)) =>
        inA_pe    := sngSig.get
        inA_exp   := sngExp.get
        inA_sign  := sngSign.get
        in_a_mask := sngMask.get
        nanA      := sngNaN.get
      case (Some(_), Some(_)) =>
        val selSingle = actType.sig === 4.U
        inA_pe    := Mux(selSingle, sngSig.get, dualSig.get)
        inA_exp   := Mux(selSingle, sngExp.get, dualExp.get)
        inA_sign  := Mux(selSingle, sngSign.get, dualSign.get)
        in_a_mask := Mux(selSingle, sngMask.get, dualMask.get)
        nanA      := Mux(selSingle, sngNaN.get,  dualNaN.get)
      case (None, None) =>
        throw new IllegalStateException("MxFpMul: actFormats is empty")
    }
  }

  // --- Weight classifier -----------------------------------------------------
  {
    val slotSigDual = config.inPE_wei_totalWidth / 2
    val slotExpDual = (config.inWeiBusWidth - config.inPE_wei_totalWidth) / 2
    val slotSigSng  = config.inPE_wei_totalWidth
    val slotExpSng  = config.inWeiBusWidth - config.inPE_wei_totalWidth

    val dualRes = if (dualWeiFormats.nonEmpty) Some(
      (0 until 2).map { i =>
        classifyAndPackSlot(lanes2_w(i), weiType, dualWeiFormats, slotSigDual, slotExpDual)
      }
    ) else None

    val sngRes = if (singleWeiFormats.nonEmpty) Some(
      classifyAndPackSlot(lanes1_w(0), weiType, singleWeiFormats, slotSigSng, slotExpSng)
    ) else None

    val dualSig  = dualRes.map(r => Cat(r(1)._1, r(0)._1))
    val dualExp  = dualRes.map(r => Cat(r(1)._2, r(0)._2))
    val dualSign = dualRes.map(r => Cat(r(1)._3, r(0)._3))
    val dualMask = dualRes.map(r => Cat(r(1)._4.asUInt, r(0)._4.asUInt))
    val dualNaN  = dualRes.map(r => r(0)._5 || r(1)._5)

    val sngSig   = sngRes.map(_._1)
    val sngExp   = sngRes.map(_._2)
    val sngSign  = sngRes.map(r => Fill(2, r._3))
    val sngMask  = sngRes.map(r => Fill(2, r._4.asUInt))
    val sngNaN   = sngRes.map(_._5)

    (dualRes, sngRes) match {
      case (Some(_), None) =>
        inW_pe    := dualSig.get
        inW_exp   := dualExp.get
        inW_sign  := dualSign.get
        in_w_mask := dualMask.get
        nanW      := dualNaN.get
      case (None, Some(_)) =>
        inW_pe    := sngSig.get
        inW_exp   := sngExp.get
        inW_sign  := sngSign.get
        in_w_mask := sngMask.get
        nanW      := sngNaN.get
      case (Some(_), Some(_)) =>
        val selSingle = weiType.sig === 4.U
        inW_pe    := Mux(selSingle, sngSig.get, dualSig.get)
        inW_exp   := Mux(selSingle, sngExp.get, dualExp.get)
        inW_sign  := Mux(selSingle, sngSign.get, dualSign.get)
        in_w_mask := Mux(selSingle, sngMask.get, dualMask.get)
        nanW      := Mux(selSingle, sngNaN.get,  dualNaN.get)
      case (None, None) =>
        throw new IllegalStateException("MxFpMul: weiFormats is empty")
    }
  }

  // Compute the sign of the outputs
  val out_signs = Cat(inA_sign(1) ^ inW_sign(1),
                      inA_sign(1) ^ inW_sign(0),
                      inA_sign(0) ^ inW_sign(1),
                      inA_sign(0) ^ inW_sign(0))
  // PE Instantiation
  val out_pe = Wire(UInt(config.outPE_width.W))
  val PE = Module(new MxPE(config, lut))
  PE.io.modeDecoded := modeWire
  PE.io.enable := io.enable
  PE.io.mask_a := ~in_a_mask.asUInt
  PE.io.mask_w := ~in_w_mask.asUInt
  PE.io.in_a := inA_pe
  PE.io.in_w := inW_pe
  out_pe := PE.io.output

  // Exp Adder Instantiation
  val out_e = Wire(UInt(totalAdderWidth.W))
  val expAdder = Module(new MxExp(inA_exp_width = config.inActBusWidth - config.inPE_act_totalWidth, inW_exp_width = config.inWeiBusWidth - config.inPE_wei_totalWidth, outWidth = totalAdderWidth, elemW = config.expAdderWidths, outTypes = Seq(outType1, outType4, outType1, outType4)))
  expAdder.io.enable := io.enable
  expAdder.io.modeDecoded := modeWire
  expAdder.io.mask_a := ~in_a_mask.asUInt
  expAdder.io.mask_w := ~in_w_mask.asUInt
  expAdder.io.in_a := inA_exp
  expAdder.io.in_w := inW_exp
  out_e := expAdder.io.out_exp

  // PE product → MxPEAddRecFN: slice raw peMag per output mode, MSB-align to
  // sigWidth, and feed straight in. The postMul CLZ absorbs non-normalization.
  val productFmt  = config.productFormat
  val outSig      = productFmt.sig
  val outExp      = productFmt.exp
  val outBias     = productFmt.bias
  val laneExpWidth = outExp + 1

  // MSB-align an n-bit raw mult slice into a cType.sig-bit field. When
  // cType.sig >= n we pad LSBs with zeros; when cType.sig < n we truncate the
  // low (n - cType.sig) bits of the raw output, since the accumulator can't
  // represent them anyway.
  def alignToAccSig(base: Int, n: Int): UInt =
    if (cType.sig >= n) Cat(out_pe(base + n - 1, base), 0.U((cType.sig - n).W))
    else                out_pe(base + n - 1, base + n - cType.sig)

  // Build a cType.sig-bit peMag from the raw out_pe slice; runtime mux on
  // actType.sig/weiType.sig picks the width that matches the mode.
  def peMagOut4(laneIdx: Int): UInt = {
    val laneW = config.outPE_width / 4
    val base  = laneIdx * laneW
    val pairs = config.modesSupported.filter(_.numOutputs == 4)
                      .map(m => (m.actWidth, m.weiWidth)).toSet
    val need6 = laneW >= 6 && pairs.contains((3, 3))
    val need5 = laneW >= 5 && (pairs.contains((2, 3)) || pairs.contains((3, 2)))
    val s4    = alignToAccSig(base, 4)
    val m6    = if (need6) Some(alignToAccSig(base, 6)) else None
    val m5    = if (need5) Some(alignToAccSig(base, 5)) else None
    (m6, m5) match {
      case (Some(v6), Some(v5)) =>
        Mux(actType.sig === 2.U && weiType.sig === 2.U, s4,
          Mux(actType.sig === 3.U && weiType.sig === 3.U, v6, v5))
      case (Some(v6), None) =>
        Mux(actType.sig === 3.U && weiType.sig === 3.U, v6, s4)
      case (None, Some(v5)) =>
        Mux(actType.sig === 2.U && weiType.sig === 2.U, s4, v5)
      case (None, None) => s4
    }
  }

  def peMagOut2(laneIdx: Int): UInt = {
    val halfW = config.outPE_width / 2
    val base  = laneIdx * halfW
    val need7 = halfW >= 7
    val s6    = alignToAccSig(base, 6)
    val m7    = if (need7) Some(alignToAccSig(base, 7)) else None
    m7 match {
      case Some(v7) =>
        Mux(actType.sig === 2.U || weiType.sig === 2.U, s6, v7)
      case None => s6
    }
  }

  def peMagOut1: UInt = out_pe(outSig - 1, 0)

  // latency=2 splits as: one reg before MxPEAddRecFN (registers peMag/peExp/...
  // /c on the wires below) plus one reg inside it (toPostMul/mulAddResult).
  // The fpnew variant maps latency directly to the BlackBox's NumPipeRegs and
  // does not use the pre/inside split.
  val addLatency  = if (latency >= 1) 1 else 0
  val preAddRegs  = if (latency >= 2) 1 else 0
  if (config.useFpnewAdder) {
    require(config.accFormat.exp == 8 && config.accFormat.sig == 8,
      s"useFpnewAdder requires accFormat=BF16(8,8); got ${config.accFormat}")
  }
  val addUnits: Option[Seq[hardfloatHelper.MxPEAddRecFN]] =
    if (config.useFpnewAdder) None
    else Some(Seq.fill(config.numActiveOutputLanes)(
      Module(new hardfloatHelper.MxPEAddRecFN(cType.exp, cType.sig, laneExpWidth, outBias, addLatency))))
  val outputs = Wire(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  val peIsNaN = nanA || nanW
  val recIn_c = io.rec_c.asTypeOf(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  // Per-lane PE outputs captured for either adder path.
  val peMagW  = Wire(Vec(config.numActiveOutputLanes, UInt(cType.sig.W)))
  val peExpW  = Wire(Vec(config.numActiveOutputLanes, UInt(laneExpWidth.W)))
  val peSignW = Wire(Vec(config.numActiveOutputLanes, Bool()))
  val peZeroW = Wire(Vec(config.numActiveOutputLanes, Bool()))

  for (i <- 0 until config.numActiveOutputLanes) {
    // Per-output-mode peMag and peExp slices. Lane indexing mirrors the
    // previous out4_toRec(i) / out2_toRec(i/2) / out1_toRec(0) scheme.
    val peMag4 = if (config.needsOut4) Some(peMagOut4(i))         else None
    val peMag2 = if (config.needsOut2) Some(peMagOut2(i / 2))     else None
    val peMag1 = if (config.needsOut1) Some(peMagOut1)            else None
    val peExp4 = if (config.needsOut4) Some(out_e((i + 1) * (totalAdderWidth / 4) - 1, i * (totalAdderWidth / 4))) else None
    val peExp2 = if (config.needsOut2) Some(out_e((i / 2) * (totalAdderWidth / 2) + outExp, (i / 2) * (totalAdderWidth / 2))) else None
    val peExp1 = if (config.needsOut1) Some(out_e(outExp, 0))     else None
    val peSign4 = out_signs(i)
    val peSign2 = out_signs((i / 2) * 2)
    val peSign1 = out_signs(0)
    val peZero4 = in_a_mask(i / 2) || in_w_mask(i % 2)
    val peZero2 = in_a_mask(i / 2) || in_w_mask((i / 2) % 2)
    val peZero1 = in_a_mask(0) || in_w_mask(0)

    val (peMag, peExp, peSign, peZero): (UInt, UInt, Bool, Bool) = config.fixedNumOutputs match {
      case Some(4) => (peMag4.get, peExp4.get, peSign4, peZero4)
      case Some(2) => (peMag2.get, peExp2.get, peSign2, peZero2)
      case Some(1) => (peMag1.get, peExp1.get, peSign1, peZero1)
      case Some(n) => throw new IllegalArgumentException(s"Unsupported fixedNumOutputs=$n")
      case None =>
        val needs4  = config.needsOut4
        val needs2  = config.needsOut2
        val needs1  = config.needsOut1
        val is4     = modeWire.numOutputs === 4.U
        val is2     = modeWire.numOutputs === 2.U
        val magSel: UInt = (needs4, needs2, needs1) match {
          case (true, true, true)  => Mux(is4, peMag4.get, Mux(is2, peMag2.get, peMag1.get))
          case (true, true, false) => Mux(is4, peMag4.get, peMag2.get)
          case (true, false, true) => Mux(is4, peMag4.get, peMag1.get)
          case (false, true, true) => Mux(is2, peMag2.get, peMag1.get)
          case _ => throw new IllegalArgumentException("Unreachable: fixedNumOutputs==None requires multiple needs*")
        }
        val expSel: UInt = (needs4, needs2, needs1) match {
          case (true, true, true)  => Mux(is4, peExp4.get, Mux(is2, peExp2.get, peExp1.get))
          case (true, true, false) => Mux(is4, peExp4.get, peExp2.get)
          case (true, false, true) => Mux(is4, peExp4.get, peExp1.get)
          case (false, true, true) => Mux(is2, peExp2.get, peExp1.get)
          case _ => throw new IllegalArgumentException("Unreachable")
        }
        val signSel: Bool = (needs4, needs2, needs1) match {
          case (true, true, true)  => Mux(is4, peSign4, Mux(is2, peSign2, peSign1))
          case (true, true, false) => Mux(is4, peSign4, peSign2)
          case (true, false, true) => Mux(is4, peSign4, peSign1)
          case (false, true, true) => Mux(is2, peSign2, peSign1)
          case _ => throw new IllegalArgumentException("Unreachable")
        }
        val zeroSel: Bool = (needs4, needs2, needs1) match {
          case (true, true, true)  => Mux(is4, peZero4, Mux(is2, peZero2, peZero1))
          case (true, true, false) => Mux(is4, peZero4, peZero2)
          case (true, false, true) => Mux(is4, peZero4, peZero1)
          case (false, true, true) => Mux(is2, peZero2, peZero1)
          case _ => throw new IllegalArgumentException("Unreachable")
        }
        (magSel, expSel, signSel, zeroSel)
    }

    // Capture for the adder stage (either hardfloat MxPEAddRecFN or fpnew BB).
    peMagW(i)  := peMag
    peExpW(i)  := peExp
    peSignW(i) := peSign
    peZeroW(i) := peZero

    // latency=1 reg lives at mulAddResult inside MxPEAddRecFN (same split as
    // stock MulAddRecFNPipe). latency=2 adds another reg here, before the adder,
    // so the pipeline is: PE/Exp | rawC+align+mulAddResult | postMul+round.
    if (!config.useFpnewAdder) {
      def pipe[T <: chisel3.Data](sig: T, n: Int): T =
        (0 until n).foldLeft(sig)((s, _) => RegNext(s))
      val unit = addUnits.get(i)
      unit.io.roundingMode   := hardfloat.consts.round_near_even
      unit.io.detectTininess := hardfloat.consts.tininess_afterRounding
      unit.io.peMag    := pipe(peMag,     preAddRegs)
      unit.io.peExp    := pipe(peExp,     preAddRegs)
      unit.io.peSign   := pipe(peSign,    preAddRegs)
      unit.io.peIsZero := pipe(peZero,    preAddRegs)
      unit.io.peIsNaN  := pipe(peIsNaN,   preAddRegs)
      unit.io.c        := pipe(recIn_c(i), preAddRegs)

      outputs(i) := unit.io.out
    }
  }

  // ---- fpnew/cvfpu adder path (BF16 only) -----------------------------------
  // Round each lane's raw PE product (peMag/peExp/peSign + isZero/isNaN) to
  // IEEE BF16 via a (peExpWidth, peMagWidth) RawFloat → recoded BF16 round →
  // IEEE BF16 cast. The cast widens product precision into BF16 — for configs
  // where productFormat ≠ BF16 (e.g., mxgemmini's E4M3 product), peExp is only
  // peExpWidth bits and biased by productFormat.bias, so we can't reuse the
  // BF16-shaped MxPEOutToRaw helper directly.
  //
  // peMag is `cType.sig`-bit MSB-aligned; we count leading zeros, normalize
  // to put the implicit-1 at bit (peMagWidth-1), and compute the recoded sExp
  // sExp = peExp - productBias - normDist + 2^peExpWidth (hardfloat's recoded
  // sExp == unbiased + 2^inExpWidth — independent of bias).
  if (config.useFpnewAdder) {
    val numLanes     = config.numActiveOutputLanes
    val productBias  = config.productFormat.bias
    val peMagWidth   = cType.sig
    val peExpWidth   = laneExpWidth                    // = productFormat.exp + 1
    val sExpW        = peExpWidth + 2                  // RawFloat.sExp width
    val recodedOff   = (BigInt(1) << peExpWidth) - BigInt(productBias)

    def peProductToBf16Ieee(
      peMag: UInt, peExp: UInt, peSign: Bool, peIsZero: Bool, peIsNaN: Bool
    ): UInt = {
      val isZero   = peIsZero || (peMag === 0.U)
      val normDist = countLeadingZeros(peMag)
      // Shift peMag left so the leading-1 lands at bit (peMagWidth-1).
      val normMag  = (peMag << normDist)(peMagWidth - 1, 0)
      // sExp computed in UInt domain (always non-negative for our value range)
      // then reinterpreted as SInt to match RawFloat.sExp.
      val sExpUInt = peExp +& recodedOff.U(sExpW.W) - normDist
      val sExpSInt = sExpUInt(sExpW - 1, 0).asSInt
      // sig: top 2 bits cannot both be 0; convention is "0_<implicit_1>_<frac>".
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
  def apply(expWidth: Int, sigWidth: Int, sign: UInt, exp: UInt, sig: UInt, inputNaN: Bool, inputZero: Bool): RawFloat = {
    val expIn = exp(expWidth, 0)
    val fractIn = sig(sigWidth-2, 0)

    val isZeroExpIn = (expIn === 0.U)
    val isZeroFractIn = (fractIn === 0.U)

    val normDist = countLeadingZeros(fractIn)
    val subnormFract = if (sigWidth > 2) {
      (fractIn << normDist) (sigWidth - 3, 0) << 1
    } else {
      0.U
    }
    val adjustedExp =
      Mux(isZeroExpIn,
        normDist ^ ((BigInt(1) << (expWidth + 1)) - 1).U,
        expIn
      ) + ((BigInt(1) << (expWidth - 1)).U
        | Mux(isZeroExpIn, 2.U, 1.U))

    val isZero = isZeroExpIn && isZeroFractIn
    val isSpecial = adjustedExp(expWidth, expWidth - 1) === 3.U

    val satExp  = (BigInt(3) << (expWidth - 1)).U((expWidth + 1).W)
    val satFrac = ((BigInt(1) << (sigWidth - 1)) - 2).U((sigWidth - 1).W)
    val saturate     = isSpecial && !inputNaN
    val effectiveZero = (isZero || inputZero) && !inputNaN

    val out = Wire(new RawFloat(expWidth, sigWidth))
    out.isNaN  := inputNaN
    out.isInf  := false.B
    out.isZero := effectiveZero
    out.sign   := sign
    out.sExp   := Mux(inputNaN || saturate, satExp, adjustedExp(expWidth, 0)).zext
    out.sig    := 0.U(1.W) ## !effectiveZero ## Mux(inputNaN || saturate, satFrac, Mux(isZeroExpIn, subnormFract, fractIn))
    out
  }
}

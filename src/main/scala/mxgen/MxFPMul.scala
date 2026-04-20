package mxgen

import chisel3._
import chisel3.util._
import mxgen.hardfloat._

class MxFpMul(val config: MxConfig, lut: Boolean) extends Module {
  println("Creating MxFpMul with product precision: " + config.productFormat + " and acc precision: " + config.accFormat)
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

  // ---------------------------------------------------------------------------
  // Classify input lanes.
  // Each block is guarded at elaboration time by the per-format support flag.
  // The `when` guards use `actType`/`weiType` which are hardwired constants
  // for single-format configs, enabling FIRRTL to eliminate dead dispatch muxes.
  // ---------------------------------------------------------------------------
  if (config.actSupportFp4) {
    val in_a_w2_cl = lanes2_a.map { f => classify(MxFormat.FP4, f(3, 0)) }
    val (exps_a_w2, sigs_a_w2, signs_a_w2) = in_a_w2_cl.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i*2), config.inPE_act_totalWidth / 2, (config.inActBusWidth - config.inPE_act_totalWidth)/2) }.unzip3
    val in_a_w2_zero = in_a_w2_cl.map(f => f.isZero)

    when (actType.sig === 2.U) {
      inA_pe := VecInit(sigs_a_w2).asUInt
      inA_exp := VecInit(exps_a_w2).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w2(i) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w2_zero(i)}.asUInt
    }
  }
  if (config.actSupportFp6_1) {
    val in_a_w3_cl_fp6 = lanes2_a.map { f => classify(MxFormat.FP6_E3M2, f(5, 0)) }
    val (exps_a_w3_fp6_1, sigs_a_w3_fp6_1, signs_a_w3_fp6_1) = in_a_w3_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i*2), config.inPE_act_totalWidth / 2, (config.inActBusWidth - config.inPE_act_totalWidth)/2) }.unzip3
    val in_a_w3_zero_fp6 = in_a_w3_cl_fp6.map(f => f.isZero)

    when (actType.exp === 3.U && actType.sig === 3.U) {
      inA_pe := VecInit(sigs_a_w3_fp6_1).asUInt
      inA_exp := VecInit(exps_a_w3_fp6_1).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w3_fp6_1(i) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w3_zero_fp6(i)}.asUInt
    }
  }
  if (config.actSupportFp8_1) {
    val in_a_w3_cl_fp8 = lanes2_a.map { f => classify(MxFormat.FP8_E5M2, f(7, 0)) }
    val (exps_a_w3_fp8_1, sigs_a_w3_fp8_1, signs_a_w3_fp8_1) = in_a_w3_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i*2), config.inPE_act_totalWidth / 2, (config.inActBusWidth - config.inPE_act_totalWidth)/2) }.unzip3
    val in_a_w3_zero_fp8 = in_a_w3_cl_fp8.map(f => f.isZero)

    when (actType.exp === 5.U && actType.sig === 3.U) {
      inA_pe := VecInit(sigs_a_w3_fp8_1).asUInt
      inA_exp := VecInit(exps_a_w3_fp8_1).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w3_fp8_1(i) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w3_zero_fp8(i)}.asUInt
      nanA := in_a_w3_cl_fp8(0).isNaN || in_a_w3_cl_fp8(1).isNaN
    }
  }
  if (config.actSupportFp6_0) {
    val in_a_w4_cl_fp6 = lanes1_a.map { f => classify(MxFormat.FP6_E2M3, f(5, 0)) }
    val (exps_a_w4_fp6_0, sigs_a_w4_fp6_0, signs_a_w4_fp6_0) = in_a_w4_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i*2), config.inPE_act_totalWidth, config.inActBusWidth - config.inPE_act_totalWidth) }.unzip3
    val in_a_w4_zero_fp6 = in_a_w4_cl_fp6.map(f => f.isZero)

    when (actType.exp === 2.U && actType.sig === 4.U) {
      inA_pe := VecInit(sigs_a_w4_fp6_0).asUInt
      inA_exp := VecInit(exps_a_w4_fp6_0).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w4_fp6_0(0) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w4_zero_fp6(0)}.asUInt
    }
  }
  if (config.actSupportFp8_0) {
    val in_a_w4_cl_fp8 = lanes1_a.map { f => classify(MxFormat.FP8_E4M3, f(7, 0)) }
    val (exps_a_w4_fp8_0, sigs_a_w4_fp8_0, signs_a_w4_fp8_0) = in_a_w4_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i*2), config.inPE_act_totalWidth, config.inActBusWidth - config.inPE_act_totalWidth) }.unzip3
    val in_a_w4_zero_fp8 = in_a_w4_cl_fp8.map(f => f.isZero)

    when (actType.exp === 4.U && actType.sig === 4.U) {
      inA_pe := VecInit(sigs_a_w4_fp8_0).asUInt
      inA_exp := VecInit(exps_a_w4_fp8_0).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w4_fp8_0(0) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w4_zero_fp8(0)}.asUInt
      nanA := in_a_w4_cl_fp8(0).isNaN
    }
  }

  if (config.weiSupportFp4) {
    val in_w_w2_cl = lanes2_w.map { f => classify(MxFormat.FP4, f(3, 0)) }
    val (exps_w_w2, sigs_w_w2, signs_w_w2) = in_w_w2_cl.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i), config.inPE_wei_totalWidth / 2, (config.inWeiBusWidth - config.inPE_wei_totalWidth)/2) }.unzip3
    val in_w_w2_zero = in_w_w2_cl.map(f => f.isZero)

    when (weiType.sig === 2.U) {
      inW_pe := VecInit(sigs_w_w2).asUInt
      inW_exp := VecInit(exps_w_w2).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w2(i) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w2_zero(i)}.asUInt
    }
  }
  if (config.weiSupportFp6_1) {
    val in_w_w3_cl_fp6 = lanes2_w.map { f => classify(MxFormat.FP6_E3M2, f(5, 0)) }
    val (exps_w_w3_fp6_1, sigs_w_w3_fp6_1, signs_w_w3_fp6_1) = in_w_w3_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i), config.inPE_wei_totalWidth / 2, (config.inWeiBusWidth - config.inPE_wei_totalWidth)/2) }.unzip3
    val in_w_w3_zero_fp6 = in_w_w3_cl_fp6.map(f => f.isZero)

    when (weiType.exp === 3.U && weiType.sig === 3.U) {
      inW_pe := VecInit(sigs_w_w3_fp6_1).asUInt
      inW_exp := VecInit(exps_w_w3_fp6_1).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w3_fp6_1(i) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w3_zero_fp6(i)}.asUInt
    }
  }
  if (config.weiSupportFp8_1) {
    val in_w_w3_cl_fp8 = lanes2_w.map { f => classify(MxFormat.FP8_E5M2, f(7, 0)) }
    val (exps_w_w3_fp8_1, sigs_w_w3_fp8_1, signs_w_w3_fp8_1) = in_w_w3_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i), config.inPE_wei_totalWidth / 2, (config.inWeiBusWidth - config.inPE_wei_totalWidth)/2) }.unzip3
    val in_w_w3_zero_fp8 = in_w_w3_cl_fp8.map(f => f.isZero)

    when (weiType.exp === 5.U && weiType.sig === 3.U) {
      inW_pe := VecInit(sigs_w_w3_fp8_1).asUInt
      inW_exp := VecInit(exps_w_w3_fp8_1).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w3_fp8_1(i) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w3_zero_fp8(i)}.asUInt
      nanW := in_w_w3_cl_fp8(0).isNaN || in_w_w3_cl_fp8(1).isNaN
    }
  }
  if (config.weiSupportFp6_0) {
    val in_w_w4_cl_fp6 = lanes1_w.map { f => classify(MxFormat.FP6_E2M3, f(5, 0)) }
    val (exps_w_w4_fp6_0, sigs_w_w4_fp6_0, signs_w_w4_fp6_0) = in_w_w4_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i), config.inPE_wei_totalWidth, config.inWeiBusWidth - config.inPE_wei_totalWidth) }.unzip3
    val in_w_w4_zero_fp6 = in_w_w4_cl_fp6.map(f => f.isZero)

    when (weiType.exp === 2.U && weiType.sig === 4.U) {
      inW_pe := VecInit(sigs_w_w4_fp6_0).asUInt
      inW_exp := VecInit(exps_w_w4_fp6_0).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w4_fp6_0(0) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w4_zero_fp6(0)}.asUInt
    }
  }
  if (config.weiSupportFp8_0) {
    val in_w_w4_cl_fp8 = lanes1_w.map { f => classify(MxFormat.FP8_E4M3, f(7, 0)) }
    val (exps_w_w4_fp8_0, sigs_w_w4_fp8_0, signs_w_w4_fp8_0) = in_w_w4_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, config.expAdderWidths(i), config.inPE_wei_totalWidth, config.inWeiBusWidth - config.inPE_wei_totalWidth) }.unzip3
    val in_w_w4_zero_fp8 = in_w_w4_cl_fp8.map(f => f.isZero)

    when (weiType.exp === 4.U && weiType.sig === 4.U) {
      inW_pe := VecInit(sigs_w_w4_fp8_0).asUInt
      inW_exp := VecInit(exps_w_w4_fp8_0).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w4_fp8_0(0) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w4_zero_fp8(0)}.asUInt
      nanW := in_w_w4_cl_fp8(0).isNaN
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

  // ---------------------------------------------------------------------------
  // Normalize paths — only generated for numOutputs values actually present.
  // Each path converts integer PE products back to floating-point (RawFloat).
  // ---------------------------------------------------------------------------
  // For the 4-output path, the PE lane width determines which normalizer sizes
  // are valid. Only generate normalizers that fit within the lane.
  val out4LaneWidth = config.outPE_width / 4
  // Only generate a normalizer size if a 4-output mode actually produces that
  // product width. Previously we gated on per-format support union, which
  // over-instantiated when the configured mode set never pairs those widths
  // (e.g. mxgemmini = {mode0, mode4, mode8} has no 2×3 / 3×2 products).
  val out4SigPairs: Set[(Int, Int)] =
    config.modesSupported.filter(_.numOutputs == 4).map(m => (m.actWidth, m.weiWidth)).toSet
  val need4Norm6 = config.needsOut4 && out4LaneWidth >= 6 &&
    out4SigPairs.contains((3, 3))  // 3×3 products
  val need4Norm5 = config.needsOut4 && out4LaneWidth >= 5 &&
    (out4SigPairs.contains((2, 3)) || out4SigPairs.contains((3, 2)))  // mixed 2×3/3×2

  val out4_toRec: Option[Vec[RawFloat]] = if (config.needsOut4) Some(VecInit.tabulate(4) { i =>
    val n1 = if (need4Norm6) Some(normalize(out_pe(i*(config.outPE_width/4) + 5, i*config.outPE_width/4), outType4.sig - 1, 6)) else None
    val n2 = if (need4Norm5) Some(normalize(out_pe(i*(config.outPE_width/4) + 4, i*config.outPE_width/4), outType4.sig - 1, 5)) else None
    val n3 = normalize(out_pe(i*(config.outPE_width/4) + 3, i*config.outPE_width/4), outType4.sig - 1, 4)

    // Select normalizer result based on which sizes are available
    val (out4_rec_exp, shift_dir, out4_rec_sig) = (n1, n2) match {
      case (Some(v1), Some(v2)) =>
        (Mux(actType.sig === 2.U && weiType.sig === 2.U, n3._2, Mux(actType.sig === 3.U && weiType.sig === 3.U, v1._2, v2._2)),
         Mux(actType.sig === 2.U && weiType.sig === 2.U, n3._3, Mux(actType.sig === 3.U && weiType.sig === 3.U, v1._3, v2._3)),
         Mux(actType.sig === 2.U && weiType.sig === 2.U, n3._1, Mux(actType.sig === 3.U && weiType.sig === 3.U, v1._1, v2._1)))
      case (Some(v1), None) =>
        (Mux(actType.sig === 3.U && weiType.sig === 3.U, v1._2, n3._2),
         Mux(actType.sig === 3.U && weiType.sig === 3.U, v1._3, n3._3),
         Mux(actType.sig === 3.U && weiType.sig === 3.U, v1._1, n3._1))
      case (None, Some(v2)) =>
        (Mux(actType.sig === 2.U && weiType.sig === 2.U, n3._2, v2._2),
         Mux(actType.sig === 2.U && weiType.sig === 2.U, n3._3, v2._3),
         Mux(actType.sig === 2.U && weiType.sig === 2.U, n3._1, v2._1))
      case (None, None) =>
        (n3._2, n3._3, n3._1)
    }

    MxPEOutToRaw(
      expWidth = outType4.exp,
      sigWidth = outType4.sig,
      sign = out_signs(i),
      exp = Mux(!shift_dir, out_e((i+1)*(totalAdderWidth/4)-1, i*(totalAdderWidth/4)) -% out4_rec_exp, out_e((i+1)*(totalAdderWidth/4)-1, i*(totalAdderWidth/4)) +% out4_rec_exp),
      sig = out4_rec_sig,
      inputNaN  = nanA || nanW,
      inputZero = in_a_mask(i / 2) || in_w_mask(i % 2)
    )
  }) else None

  val out2HalfWidth = config.outPE_width / 2
  val need2Norm7 = config.needsOut2 && out2HalfWidth >= 7

  val out2_toRec: Option[Vec[RawFloat]] = if (config.needsOut2) Some(VecInit.tabulate(2) { i =>
    val n1 = if (need2Norm7) Some(normalize(out_pe(i*(config.outPE_width/2) + 6, i*config.outPE_width/2), outType2.sig - 1, 7)) else None
    val n2 = normalize(out_pe(i*(config.outPE_width/2) + 5, i*config.outPE_width/2), outType2.sig - 1, 6)

    val (out2_toRec_exp, shift_dir, out2_toRec_sig) = n1 match {
      case Some(v1) =>
        (Mux(actType.sig === 2.U || weiType.sig === 2.U, n2._2, v1._2),
         Mux(actType.sig === 2.U || weiType.sig === 2.U, n2._3, v1._3),
         Mux(actType.sig === 2.U || weiType.sig === 2.U, n2._1, v1._1))
      case None =>
        (n2._2, n2._3, n2._1)
    }

    MxPEOutToRaw(
      expWidth = outType2.exp,
      sigWidth = outType2.sig,
      sign = out_signs(i*2),
      exp = Mux(!shift_dir, out_e(i*(totalAdderWidth/2) + outType2.exp, i*(totalAdderWidth/2)) -% out2_toRec_exp, out_e(i*(totalAdderWidth/2) + outType2.exp, i*(totalAdderWidth/2)) +% out2_toRec_exp),
      sig = out2_toRec_sig,
      inputNaN  = nanA || nanW,
      inputZero = in_a_mask(i) || in_w_mask(i % 2)
    )
  }) else None

  val out1_toRec: Option[Vec[RawFloat]] = if (config.needsOut1) Some(VecInit.tabulate(1) { i =>
    val out1_toRec_norm = normalize(out_pe(7,0), outType1.sig - 1, 8)

    MxPEOutToRaw(
      expWidth = outType1.exp,
      sigWidth = outType1.sig,
      sign = out_signs(0),
      exp = Mux(!out1_toRec_norm._3, out_e((i)*(totalAdderWidth) + outType1.exp, i*(totalAdderWidth)) -% out1_toRec_norm._2, out_e((i)*(totalAdderWidth) + outType1.exp, i*(totalAdderWidth)) +% out1_toRec_norm._2),
      sig = out1_toRec_norm._1,
      inputNaN  = nanA || nanW,
      inputZero = in_a_mask(0) || in_w_mask(0)
    )
  }) else None

  // ---------------------------------------------------------------------------
  // Add units — only `numActiveOutputLanes` instantiated.
  // MxMulAddRecFN is the add side of a fused FMA (no actual multiplier). It
  // accepts rawA directly at productFormat widths and widens inline, so the
  // product→adder path is pure wiring plus a constant exp-bias shift and a
  // fraction zero-pad. No per-lane RoundAnyRawFNToRecFN is needed.
  // ---------------------------------------------------------------------------
  val productFmt = config.productFormat
  val addUnits = Seq.fill(config.numActiveOutputLanes)(
    Module(new hardfloatHelper.MxMulAddRecFN(cType.exp, cType.sig, productFmt.exp, productFmt.sig)))
  val outputs = Wire(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  // outType1/2/4 are all `config.productFormat`, so the RawFloats coming out
  // of every out*_toRec path share a format and can be muxed directly.
  for (i <- 0 until config.numActiveOutputLanes) {
    val rawAtProduct: RawFloat = config.fixedNumOutputs match {
      case Some(4) => out4_toRec.get(i)
      case Some(2) => out2_toRec.get(i)
      case Some(1) => out1_toRec.get(0)
      case Some(n) => throw new IllegalArgumentException(s"Unsupported fixedNumOutputs=$n")
      case None =>
        // Multiple numOutputs values — build mux with only available branches.
        // Use original indexing: out4 lane=i, out2 lane=i/2, out1 lane=0.
        if (config.needsOut4 && config.needsOut2 && config.needsOut1) {
          Mux(modeWire.numOutputs === 4.U, out4_toRec.get(i),
            Mux(modeWire.numOutputs === 2.U, out2_toRec.get(i/2),
              out1_toRec.get(0)))
        } else if (config.needsOut4 && config.needsOut2) {
          Mux(modeWire.numOutputs === 4.U, out4_toRec.get(i),
            out2_toRec.get(i/2))
        } else if (config.needsOut4 && config.needsOut1) {
          Mux(modeWire.numOutputs === 4.U, out4_toRec.get(i),
            out1_toRec.get(0))
        } else { // needsOut2 && needsOut1
          Mux(modeWire.numOutputs === 2.U, out2_toRec.get(i/2),
            out1_toRec.get(0))
        }
    }
    val recIn_c = io.rec_c.asTypeOf(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))(i)

    addUnits(i).io.roundingMode := hardfloat.consts.round_near_even
    addUnits(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    addUnits(i).io.a := rawAtProduct
    addUnits(i).io.c := recIn_c

    outputs(i) := addUnits(i).io.out
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

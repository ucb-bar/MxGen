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

  // Shared classify+pack for one operand element. Replaces the per-format
  // cones (one `classify` + one `pack` per format, all muxed after the fact)
  // with a single pipeline where all per-format combinational work is done
  // inside one helper, then Mux1H'd on the runtime-selected format. Since
  // formats sharing a sigWidth also share raw-sig bit positions (E3M2/E5M2;
  // E2M3/E4M3), CIRCT collapses the duplicated rawSig cones into one.
  //
  // Exp packing: the original pack() does
  //     packed_exp = c.exp.asSInt               // f.expWidth-bit SInt
  //     slot       = packed_exp.pad(slotExpWidth).asUInt
  // i.e. SIGN-extend the biased-exp bit pattern up to the slot width, not
  // zero-extend. MxExp re-reads the slot via `slot(w-1,0).asSInt`, so the
  // two-s-complement sign bit matters all the way up to w=expAdderWidths(i).
  private def classifyAndPackSlot(
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

  // ---------------------------------------------------------------------------
  // Classify input lanes.
  //
  // Formats split into two packing paths by sigWidth:
  //   * sig < 4  -> "dual"   : two elements packed into lanes2_{a,w}.
  //   * sig >= 4 -> "single" : one element filling lanes1_{a,w}; sign/mask
  //                            are broadcast to both product positions.
  //
  // Within each path we run one `classifyAndPackSlot` per element slot and
  // share the raw-field mux / isZero-isSub / bias subtract across all formats
  // that use that slot. When both paths are present for a side, the two
  // results are muxed by the top sig bit (sig === 4 picks the single path).
  // ---------------------------------------------------------------------------
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

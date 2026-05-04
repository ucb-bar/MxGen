package mxgen

import chisel3._
import chisel3.util._

// Multiplier-only portion of MxFpMul. Outputs per-lane raw products
// (peMag, peExp, peSign, peIsZero, peIsNaN) for either MxFpMul's per-lane
// MxPEAddRecFN/fpnew adders or MxDotProduct's anchor reduction tree.
class MxFpMulCore(val config: MxConfig, lut: Boolean) extends Module {
  val cType        = config.accFormat
  val productFmt   = config.productFormat
  val outSig       = productFmt.sig
  val outExp       = productFmt.exp
  val laneExpWidth = outExp + 1
  val totalAdderWidth = 4 * laneExpWidth

  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val type_a        = Input(new MxTypeBundle())
    val in_weights    = Input(UInt(config.inWeiBusWidth.W))
    val type_w        = Input(new MxTypeBundle())
    val mode          = Input(new mxMode())
    val enable        = Input(Bool())

    val peMag    = Output(Vec(config.numActiveOutputLanes, UInt(cType.sig.W)))
    val peExp    = Output(Vec(config.numActiveOutputLanes, UInt(laneExpWidth.W)))
    val peSign   = Output(Vec(config.numActiveOutputLanes, Bool()))
    val peIsZero = Output(Vec(config.numActiveOutputLanes, Bool()))
    val peIsNaN  = Output(Bool())
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
      val adjExp: UInt =
        (Mux(isSub, 1.U(f.expWidth.W), rawExp) -% f.bias.U(f.expWidth.W)) -% 1.U(1.W)
      (sign, rawSig, isNaN, isZero, isSub, adjExp)
    }

    val muxedSign:  UInt = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._1 })
    val muxedIsNaN: Bool = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._3 })
    val muxedIsZero: Bool = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._4 })
    val muxedIsSub:  Bool = Mux1H(sels.zip(perFmt).map { case (sel, t) => sel -> t._5 })

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

  val lanes2_a = io.in_activation.asTypeOf(Vec(2, UInt((config.inActBusWidth/2).W)))
  val lanes1_a = io.in_activation.asTypeOf(Vec(1, UInt((config.inActBusWidth).W)))
  val lanes2_w = io.in_weights.asTypeOf(Vec(2, UInt((config.inWeiBusWidth/2).W)))
  val lanes1_w = io.in_weights.asTypeOf(Vec(1, UInt((config.inWeiBusWidth).W)))

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
        throw new IllegalStateException("MxFpMulCore: actFormats is empty")
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
        throw new IllegalStateException("MxFpMulCore: weiFormats is empty")
    }
  }

  val out_signs = Cat(inA_sign(1) ^ inW_sign(1),
                      inA_sign(1) ^ inW_sign(0),
                      inA_sign(0) ^ inW_sign(1),
                      inA_sign(0) ^ inW_sign(0))

  val out_pe = Wire(UInt(config.outPE_width.W))
  val PE = Module(new MxPE(config, lut))
  PE.io.modeDecoded := modeWire
  PE.io.enable := io.enable
  PE.io.mask_a := ~in_a_mask.asUInt
  PE.io.mask_w := ~in_w_mask.asUInt
  PE.io.in_a := inA_pe
  PE.io.in_w := inW_pe
  out_pe := PE.io.output

  val out_e = Wire(UInt(totalAdderWidth.W))
  val expAdder = Module(new MxExp(
    inA_exp_width = config.inActBusWidth - config.inPE_act_totalWidth,
    inW_exp_width = config.inWeiBusWidth - config.inPE_wei_totalWidth,
    outWidth      = totalAdderWidth,
    elemW         = config.expAdderWidths,
    outTypes      = Seq(productFmt, productFmt, productFmt, productFmt)))
  expAdder.io.enable := io.enable
  expAdder.io.modeDecoded := modeWire
  expAdder.io.mask_a := ~in_a_mask.asUInt
  expAdder.io.mask_w := ~in_w_mask.asUInt
  expAdder.io.in_a := inA_exp
  expAdder.io.in_w := inW_exp
  out_e := expAdder.io.out_exp

  // MSB-align an n-bit raw mult slice into an accFormat.sig-bit field.
  def alignToAccSig(base: Int, n: Int): UInt =
    if (cType.sig >= n) Cat(out_pe(base + n - 1, base), 0.U((cType.sig - n).W))
    else                out_pe(base + n - 1, base + n - cType.sig)

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

  val peIsNaN = nanA || nanW

  for (i <- 0 until config.numActiveOutputLanes) {
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

    io.peMag(i)    := peMag
    io.peExp(i)    := peExp
    io.peSign(i)   := peSign
    io.peIsZero(i) := peZero
  }

  io.peIsNaN := peIsNaN
}

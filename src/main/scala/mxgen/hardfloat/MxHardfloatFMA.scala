package mxgen.hardfloat

import chisel3._
import chisel3.util._
import mxgen._

// MxHardfloatFMA — area-comparison baseline using stock hardfloat primitives.
// uniformBF16=true: MulAddRecFN(8,8) per lane; false: sized per lane.
class MxHardfloatFMA(val config: MxConfig, val uniformBF16: Boolean = true,
                     val latency: Int = 0) extends Module {
  require(latency == 0 || latency == 1,
    s"MxHardfloatFMA: latency must be 0 or 1 (got $latency)")
  println(s"Creating MxHardfloatFMA baseline (uniformBF16=$uniformBF16, latency=$latency) acc=${config.accFormat}")
  println(config.describe)

  val numLanes = config.numActiveOutputLanes
  val accFmt   = config.accFormat
  val accRec   = accFmt.recoded
  val aE = accFmt.exp; val aS = accFmt.sig

  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val type_a        = Input(new MxTypeBundle())
    val in_weights    = Input(UInt(config.inWeiBusWidth.W))
    val type_w        = Input(new MxTypeBundle())
    val mode          = Input(new mxMode())
    val enable        = Input(Bool())
    val rec_c         = Input(UInt((numLanes * accRec).W))
    val out           = Output(UInt((numLanes * accRec).W))
  })
  val _ignored = io.enable

  // Runtime selectors collapse to constants for single-format / single-mode configs.
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

  // Per-lane product format = (max exp, max sig) across formats from modes that
  // place a useful value on lane i (numOutputs > i). Narrower formats widen.
  val supportedPairs: Seq[(MxFormat, MxFormat, MxPEParams)] =
    (for {
      a <- config.actFormats.toSeq
      w <- config.weiFormats.toSeq
      m =  MxPEParams.forFormatCombo(a, w)
      if config.modesSupported.contains(m)
    } yield (a, w, m))

  // hardfloat's MulRecFN path works down to (3,3); narrower formats widen into
  // the lane product format before the Mul.
  val MinMulExp = 3
  val MinMulSig = 3
  val laneProductFormats: Seq[MxFormat] = (0 until numLanes).map { i =>
    if (uniformBF16) MxFormat(8, 8)
    else {
      // Cover every format from modes that put a checked output on lane i.
      // numOutputs=1 modes only contribute to lane 0; others are don't-care.
      val fs = supportedPairs.filter { case (_, _, m) => m.numOutputs >= 2 || i == 0 }
                             .flatMap  { case (a, w, _) => Seq(a, w) }
      if (fs.isEmpty) accFmt
      else MxFormat(
        math.max(fs.map(_.expWidth).max, MinMulExp),
        math.max(fs.map(_.sigWidth).max, MinMulSig)
      )
    }
  }
  println(s"  lane product formats: ${laneProductFormats.map(_.toString).mkString("[", ", ", "]")}")

  // Bus packing (matches MxFpMul): dual-element formats place elem 0 in the
  // low half, elem 1 in the high half. idx=1 is only requested for duals.
  def extractHalves(bus: UInt, totalW: Int, f: MxFormat, i: Int): UInt = {
    val half = totalW / 2
    val w    = f.bitWidth
    if (i == 0) bus(w - 1, 0)
    else        bus(half + w - 1, half)
  }
  def extractActLane(f: MxFormat, i: Int): UInt =
    extractHalves(io.in_activation, config.inActBusWidth, f, i)
  def extractWeiLane(f: MxFormat, i: Int): UInt =
    extractHalves(io.in_weights,    config.inWeiBusWidth, f, i)

  def recForLane(i: Int, isAct: Boolean, tE: Int, tS: Int): UInt = {
    val fmts      = if (isAct) config.actFormats else config.weiFormats
    val typeWire  = if (isAct) actType else weiType
    val extract   = (f: MxFormat) => if (isAct) extractActLane(f, i) else extractWeiLane(f, i)
    val needsRt   = if (isAct) config.needsRuntimeActType else config.needsRuntimeWeiType
    val branches  = scala.collection.mutable.ListBuffer[(Bool, UInt)]()
    val default   = 0.U((tE + tS + 1).W)

    def addBranch(f: MxFormat, cond: Bool, rec: UInt): Unit = branches += ((cond, rec))

    if (fmts.contains(MxFormat.FP4)) {
      addBranch(MxFormat.FP4,
        typeWire.exp === 2.U && typeWire.sig === 2.U,
        BaselineHelpers.fp4ToRec(extract(MxFormat.FP4), tE, tS))
    }
    if (fmts.contains(MxFormat.FP6_E3M2) && i < 2) {
      addBranch(MxFormat.FP6_E3M2,
        typeWire.exp === 3.U && typeWire.sig === 3.U,
        BaselineHelpers.fp6E3M2ToRec(extract(MxFormat.FP6_E3M2), tE, tS))
    }
    if (fmts.contains(MxFormat.FP8_E5M2) && i < 2) {
      addBranch(MxFormat.FP8_E5M2,
        typeWire.exp === 5.U && typeWire.sig === 3.U,
        BaselineHelpers.fp8E5M2ToRec(extract(MxFormat.FP8_E5M2), tE, tS))
    }
    if (fmts.contains(MxFormat.FP6_E2M3) && i == 0) {
      addBranch(MxFormat.FP6_E2M3,
        typeWire.exp === 2.U && typeWire.sig === 4.U,
        BaselineHelpers.fp6E2M3ToRec(extract(MxFormat.FP6_E2M3), tE, tS))
    }
    if (fmts.contains(MxFormat.FP8_E4M3) && i == 0) {
      addBranch(MxFormat.FP8_E4M3,
        typeWire.exp === 4.U && typeWire.sig === 4.U,
        BaselineHelpers.fp8E4M3ToRec(extract(MxFormat.FP8_E4M3), tE, tS))
    }

    branches.toList match {
      case Nil                         => default
      case (_, only) :: Nil if !needsRt => only
      case xs                          => xs.foldRight(default) { case ((c, v), acc) => Mux(c, v, acc) }
    }
  }

  // Input-lane index (0 or 1) on act/wei bus for output lane i. Collapses
  // to a constant when all supporting modes agree.
  def aInputIdxLit(i: Int, m: MxPEParams): Int =
    if (m.actInputs == 2 && m.numOutputs > 1) i >> 1 else 0
  def wInputIdxLit(i: Int, m: MxPEParams): Int = (m.weiInputs, m.numOutputs) match {
    case (2, 4) => i & 1
    case (2, 2) => i >> 1
    case _      => 0
  }

  // Modes producing a checked output on lane i: numOutputs>=2 always,
  // numOutputs=1 only on lane 0 (others are don't-care / replicate).
  def modesFor(i: Int): List[MxPEParams] =
    config.modesSupported.filter(m => m.numOutputs >= 2 || i == 0)

  // Compute the recoded a / w for output lane i, muxing over the input lane
  // indices (0 and 1) when supporting modes disagree.
  def selectRecOperand(i: Int, isAct: Boolean, tE: Int, tS: Int): UInt = {
    val supporting = modesFor(i)
    val idxs       = supporting.map(m => if (isAct) aInputIdxLit(i, m) else wInputIdxLit(i, m)).distinct
    idxs match {
      case Seq(only) => recForLane(only, isAct, tE, tS)
      case _ =>
        // Two possible input-lane indices (0 or 1). Build both and mux on mode.
        val rec0 = recForLane(0, isAct, tE, tS)
        val rec1 = recForLane(1, isAct, tE, tS)
        // Runtime condition picks between idx 0 and idx 1 per active mode.
        val useIdx1 = supporting.map { m =>
          val idx = if (isAct) aInputIdxLit(i, m) else wInputIdxLit(i, m)
          if (idx == 1) Some(m) else None
        }.flatten
        val cond: Bool = useIdx1.map { m =>
          modeWire.numOutputs === m.numOutputs.U &&
          modeWire.actInputs  === m.actInputs.U &&
          modeWire.weiInputs  === m.weiInputs.U
        }.reduce(_ || _)
        Mux(cond, rec1, rec0)
    }
  }

  // Per-lane: Mul at product format, widen, Add at accFormat; collapses to
  // fused MulAddRecFN when product format already matches acc.
  val cLanes = io.rec_c.asTypeOf(Vec(numLanes, UInt(accRec.W)))
  val outs   = Wire(Vec(numLanes, UInt(accRec.W)))

  for (i <- 0 until numLanes) {
    val pf = laneProductFormats(i)
    val aRec = selectRecOperand(i, isAct = true,  pf.exp, pf.sig)
    val wRec = selectRecOperand(i, isAct = false, pf.exp, pf.sig)
    outs(i) :=
      (if (pf.exp == aE && pf.sig == aS)
        BaselineHelpers.mulAddRecFN(aRec, wRec, cLanes(i), aE, aS, latency)
      else
        BaselineHelpers.mulThenAdd(aRec, wRec, cLanes(i), pf.exp, pf.sig, aE, aS, latency))
  }

  io.out := outs.asUInt
}

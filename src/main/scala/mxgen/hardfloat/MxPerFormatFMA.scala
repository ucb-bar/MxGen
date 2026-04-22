package mxgen.hardfloat

import chisel3._
import chisel3.util._
import mxgen._

// Per-format independent-datapath baseline. For each format in
// actFormats ∩ weiFormats we instantiate N = numOutputs_of(same-format mode)
// datapaths, each doing: recode native bits -> MulAddRecFN at accFormat.
// Same-format only: no cross-type multiplies. type_a selects the active
// format's outputs; lanes beyond that format's numOutputs replicate lane 0.
class MxPerFormatFMA(val config: MxConfig) extends Module {
  println(s"Creating MxPerFormatFMA acc=${config.accFormat}")
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
  val _ignoredMode   = io.mode
  val _ignoredEn     = io.enable
  val _ignoredTypeW  = io.type_w

  val fmts: Seq[MxFormat] =
    config.actFormats.intersect(config.weiFormats).toSeq.sortBy(_.label)
  require(fmts.nonEmpty,
    "MxPerFormatFMA: actFormats ∩ weiFormats must be non-empty (same-format only)")

  def extractLane(bus: UInt, busWidth: Int, fmt: MxFormat, idx: Int): UInt = {
    val half = busWidth / 2
    val w    = fmt.bitWidth
    if (idx == 0) bus(w - 1, 0) else bus(half + w - 1, half)
  }

  def convert(fmt: MxFormat, bits: UInt): UInt = fmt match {
    case MxFormat.FP4      => BaselineHelpers.fp4ToRec(bits, aE, aS)
    case MxFormat.FP6_E3M2 => BaselineHelpers.fp6E3M2ToRec(bits, aE, aS)
    case MxFormat.FP6_E2M3 => BaselineHelpers.fp6E2M3ToRec(bits, aE, aS)
    case MxFormat.FP8_E4M3 => BaselineHelpers.fp8E4M3ToRec(bits, aE, aS)
    case MxFormat.FP8_E5M2 => BaselineHelpers.fp8E5M2ToRec(bits, aE, aS)
    case other => throw new IllegalArgumentException(
      s"MxPerFormatFMA: unsupported format $other")
  }

  val cLanes = io.rec_c.asTypeOf(Vec(numLanes, UInt(accRec.W)))

  // Per-format output lane vectors. Count = numOutputs of the (fmt, fmt) mode.
  //   mode0 / mode4 (sig 2/2 or 3/3): numOutputs=4, outer product a[i>>1]×w[i&1]
  //   mode8        (sig 4/4)        : numOutputs=1, a[0]×w[0]
  val fmtOutputs: Seq[(MxFormat, Seq[UInt])] = fmts.map { fmt =>
    val m = MxPEParams.forFormatCombo(fmt, fmt)
    val n = m.numOutputs
    val lanes = (0 until n).map { i =>
      val aIdx = if (m.actInputs == 2 && n > 1) i >> 1 else 0
      val wIdx =
        if (m.weiInputs == 2 && n == 4) i & 1
        else if (m.weiInputs == 2 && n == 2) i >> 1
        else 0
      val aBits = extractLane(io.in_activation, config.inActBusWidth, fmt, aIdx)
      val wBits = extractLane(io.in_weights,    config.inWeiBusWidth, fmt, wIdx)
      BaselineHelpers.mulAddRecFN(
        convert(fmt, aBits), convert(fmt, wBits), cLanes(i), aE, aS)
    }
    fmt -> lanes
  }

  val needsRtType = fmts.size > 1
  val outs = Wire(Vec(numLanes, UInt(accRec.W)))
  for (i <- 0 until numLanes) {
    val branches = fmtOutputs.map { case (fmt, lanes) =>
      val src = if (i < lanes.length) lanes(i) else lanes(0)
      val cond: Bool =
        io.type_a.exp === fmt.expWidth.U && io.type_a.sig === fmt.sigWidth.U
      cond -> src
    }
    outs(i) :=
      (if (!needsRtType) branches.head._2
       else branches.foldRight(0.U(accRec.W)) { case ((c, v), acc) => Mux(c, v, acc) })
  }
  io.out := outs.asUInt
}

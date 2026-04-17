package mxgen

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// -----------------------------------------------------------------------------
// NAMED MX FORMATS (user-facing enum of supported floating-point formats)
// -----------------------------------------------------------------------------

sealed trait MxFormat {
  def expWidth: Int
  def sigWidth: Int
  def exp: Int = expWidth
  def sig: Int = sigWidth
  def ieee: Int = exp + sig
  def recoded: Int = exp + sig + 1
  def bias: Int = (1 << (expWidth - 1)) - 1
  def bitWidth: Int = expWidth + sigWidth
  def label: String
  override def toString: String = label
}
object MxFormat {
  case object FP4      extends MxFormat { val expWidth = 2; val sigWidth = 2; val label = "FP4"      }
  case object FP6_E2M3 extends MxFormat { val expWidth = 2; val sigWidth = 4; val label = "FP6_E2M3" }
  case object FP6_E3M2 extends MxFormat { val expWidth = 3; val sigWidth = 3; val label = "FP6_E3M2" }
  case object FP8_E4M3 extends MxFormat { val expWidth = 4; val sigWidth = 4; val label = "FP8_E4M3" }
  case object FP8_E5M2 extends MxFormat { val expWidth = 5; val sigWidth = 3; val label = "FP8_E5M2" }

  case class Custom(expWidth: Int, sigWidth: Int) extends MxFormat {
    val label: String = s"Custom(E${expWidth}M${sigWidth - 1})"
  }

  def apply(expWidth: Int, sigWidth: Int): MxFormat =
    all.find(f => f.expWidth == expWidth && f.sigWidth == sigWidth).getOrElse(Custom(expWidth, sigWidth))

  val all:     Set[MxFormat] = Set(FP4, FP6_E2M3, FP6_E3M2, FP8_E4M3, FP8_E5M2)
  val fp4Only: Set[MxFormat] = Set(FP4)
  val fp6:     Set[MxFormat] = Set(FP6_E2M3, FP6_E3M2)
  val fp8:     Set[MxFormat] = Set(FP8_E4M3, FP8_E5M2)
}

case class MxPEParams(
  actWidth: Int = 2,
  weiWidth: Int = 2,
  actTotalWidth: Int = 4,
  weiTotalWidth: Int = 4,
  weiInputs: Int = 2,
  actInputs: Int = 2,
  shift: Seq[Seq[Int]] = Seq.fill(2,2){0},
  outTotalWidth: Int = 16,
  numOutputs: Int = 4
)

object MxPEParams {
  def mode0 = MxPEParams()
  def mode1 = MxPEParams().copy(
    weiWidth = 3,
    weiTotalWidth = 6,
    outTotalWidth = 20
  )
  def mode2 = MxPEParams().copy(
    actTotalWidth = 4,
    weiTotalWidth = 4,
    weiWidth = 4,
    weiInputs = 1,
    shift = Seq(Seq(2,0), Seq(2,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode3 = MxPEParams().copy(
    actWidth = 3, 
    actTotalWidth = 6,
    outTotalWidth = 20 
  )
  def mode4 = MxPEParams().copy(
    actTotalWidth = 6,
    weiTotalWidth = 6,
    actWidth = 3,
    weiWidth = 3,
    outTotalWidth = 24 
  )
  def mode5 = MxPEParams().copy(
    actTotalWidth = 6,
    weiTotalWidth = 4,
    actWidth = 3,
    weiWidth = 4,
    weiInputs = 1,
    shift = Seq(Seq(2,0), Seq(2,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode6 = MxPEParams().copy(
    actTotalWidth = 4,
    weiTotalWidth = 4,
    weiInputs = 2,
    actInputs = 1,
    actWidth = 4,
    shift = Seq(Seq(2,2), Seq(0,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode7 = MxPEParams().copy(
    actTotalWidth = 4,
    weiTotalWidth = 6,
    actWidth = 4,
    weiWidth = 3,
    weiInputs = 2,
    actInputs = 1,
    shift = Seq(Seq(2,2), Seq(0,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode8 = MxPEParams().copy(
    actTotalWidth = 4,
    weiTotalWidth = 4,
    actWidth = 4,
    weiWidth = 4,
    weiInputs = 1,
    actInputs = 1,
    shift = Seq(Seq(4,2), Seq(2,0)),
    outTotalWidth = 8,
    numOutputs = 1
  )
  val allModes: List[MxPEParams] = List(mode0, mode1, mode2, mode3, mode4, mode5, mode6, mode7, mode8)
  val mxGemminiConfig: List[MxPEParams] = List(mode0, mode4, mode8)

  // The mode slot that handles a given (act significand width, wei significand
  // width) combination. This is the canonical (act, wei) -> mode table.
  def forSigWidths(actSig: Int, weiSig: Int): MxPEParams = (actSig, weiSig) match {
    case (2, 2) => mode0
    case (2, 3) => mode1
    case (2, 4) => mode2
    case (3, 2) => mode3
    case (3, 3) => mode4
    case (3, 4) => mode5
    case (4, 2) => mode6
    case (4, 3) => mode7
    case (4, 4) => mode8
    case _ => throw new IllegalArgumentException(s"No PE mode for sig widths ($actSig, $weiSig)")
  }

  def forFormatCombo(act: MxFormat, wei: MxFormat): MxPEParams =
    forSigWidths(act.sigWidth, wei.sigWidth)
}

// -----------------------------------------------------------------------------
// TOP-LEVEL CONFIG
//
// `MxConfig` is the single user-facing configuration for MxPE and MxFpMul.
// It specifies which activation and weight formats are supported, and from
// those derives the PE mode slots and all hardware-level parameters.
//
// Bus widths, exp-adder widths, and per-lane PE output widths can all be
// overridden; any field left at its default is derived from the format sets.
// -----------------------------------------------------------------------------

case class MxConfig (
  actFormats:       Set[MxFormat]              = Set(MxFormat.FP4),
  weiFormats:       Set[MxFormat]              = Set(MxFormat.FP4),
  productFormat:    MxFormat                   = MxFormat.FP8_E4M3,
  accFormat:        MxFormat                   = MxFormat.FP8_E4M3,
  inActBusWidth:    Int                        = 12,
  inWeiBusWidth:    Int                        = 12,
  expAdderWidths:   Seq[Int]                   = Seq(4, 3, 3, 3),
  laneOutputWidths: Option[Seq[Int]]           = None,
  modesOverride:    Option[List[MxPEParams]]   = None,
) {
  require(actFormats.nonEmpty, "MxConfig: actFormats must not be empty")
  require(weiFormats.nonEmpty, "MxConfig: weiFormats must not be empty")
  require(expAdderWidths.length == 4, "MxConfig.expAdderWidths must have 4 entries (one per lane)")
  laneOutputWidths.foreach(s => require(s.length == 4, "MxConfig.laneOutputWidths must have 4 entries"))
  require(inActBusWidth >= MxConfig.minBusWidth(actFormats),
    s"MxConfig: inActBusWidth=$inActBusWidth too narrow for actFormats (need >= ${MxConfig.minBusWidth(actFormats)})")
  require(inWeiBusWidth >= MxConfig.minBusWidth(weiFormats),
    s"MxConfig: inWeiBusWidth=$inWeiBusWidth too narrow for weiFormats (need >= ${MxConfig.minBusWidth(weiFormats)})")

  // PE mode slots: either explicitly overridden or derived from the cartesian
  // product of actFormats x weiFormats. Use `modesOverride` when you need
  // exact control (e.g. only same-format pairs, not all cross-format combos).
  val modesSupported: List[MxPEParams] = modesOverride.getOrElse((for {
    a <- actFormats.toSeq
    w <- weiFormats.toSeq
  } yield MxPEParams.forFormatCombo(a, w)).distinct.toList)

  // Hardware widths derived from modes
  val inPE_act_width:      Int = modesSupported.map(_.actWidth).max
  val inPE_wei_width:      Int = modesSupported.map(_.weiWidth).max
  val actflexMulInWidth:   Int = if (modesSupported.exists(_.actWidth == 3)) 3 else 2
  val weiflexMulInWidth:   Int = if (modesSupported.exists(_.weiWidth == 3)) 3 else 2
  val inPE_act_totalWidth: Int = modesSupported.map(_.actTotalWidth).max
  val inPE_wei_totalWidth: Int = modesSupported.map(_.weiTotalWidth).max
  val numWeiInputs:        Int = modesSupported.map(_.weiInputs).max

  val outPE_width: Int = laneOutputWidths.map(_.sum).getOrElse(modesSupported.map(_.outTotalWidth).max)
  val laneWidths: Seq[Int] = laneOutputWidths.getOrElse(Seq.fill(4)(outPE_width / 4))
  def laneOffset(i: Int): Int = laneWidths.take(i).sum
  def laneWidth(i: Int):  Int = laneWidths(i)

  val multOutWidth: Int =
    if (modesSupported.exists(m => m.actWidth == 3 && m.weiWidth == 3)) 6
    else if (modesSupported.exists(m =>
      (m.actWidth, m.weiWidth) == (3,2) || (m.actWidth, m.weiWidth) == (2,3) ||
      (m.actWidth, m.weiWidth) == (3,4) || (m.actWidth, m.weiWidth) == (4,3)
    )) 5
    else 4

  // ---------------------------------------------------------------------------
  // Elaboration-time gating flags — derived from modesSupported.
  // These allow MxPE and MxFpMul to skip generating hardware that is
  // unreachable for the configured set of formats/modes.
  // ---------------------------------------------------------------------------
  val numOutputsValues: Set[Int] = modesSupported.map(_.numOutputs).toSet
  val fixedNumOutputs: Option[Int] = if (numOutputsValues.size == 1) Some(numOutputsValues.head) else None
  val needsOut1: Boolean = numOutputsValues.contains(1)
  val needsOut2: Boolean = numOutputsValues.contains(2)
  val needsOut4: Boolean = numOutputsValues.contains(4)
  val numActiveOutputLanes: Int = numOutputsValues.max

  val fixedActInputs: Option[Int] = {
    val s = modesSupported.map(_.actInputs).toSet
    if (s.size == 1) Some(s.head) else None
  }
  val fixedWeiInputs: Option[Int] = {
    val s = modesSupported.map(_.weiInputs).toSet
    if (s.size == 1) Some(s.head) else None
  }

  // determines if we need mode and type selection at runtime
  val needsRuntimeActType: Boolean = actFormats.size > 1
  val needsRuntimeWeiType: Boolean = weiFormats.size > 1
  val needsRuntimeMode: Boolean = modesSupported.size > 1

  val sigWidthPairs: Set[(Int, Int)] = modesSupported.map(m => (m.actWidth, m.weiWidth)).toSet

  // Per-format support flags for conditional hardware generation in MxFpMul.
  def actSupportFp4   = actFormats.contains(MxFormat.FP4)
  def actSupportFp6_0 = actFormats.contains(MxFormat.FP6_E2M3)
  def actSupportFp6_1 = actFormats.contains(MxFormat.FP6_E3M2)
  def actSupportFp8_0 = actFormats.contains(MxFormat.FP8_E4M3)
  def actSupportFp8_1 = actFormats.contains(MxFormat.FP8_E5M2)
  def weiSupportFp4   = weiFormats.contains(MxFormat.FP4)
  def weiSupportFp6_0 = weiFormats.contains(MxFormat.FP6_E2M3)
  def weiSupportFp6_1 = weiFormats.contains(MxFormat.FP6_E3M2)
  def weiSupportFp8_0 = weiFormats.contains(MxFormat.FP8_E4M3)
  def weiSupportFp8_1 = weiFormats.contains(MxFormat.FP8_E5M2)

  def formatSupportTable: Seq[(MxFormat, MxFormat, MxPEParams)] = {
    val acts = actFormats.toSeq.sortBy(_.label)
    val weis = weiFormats.toSeq.sortBy(_.label)
    for { a <- acts; w <- weis } yield (a, w, MxPEParams.forFormatCombo(a, w))
  }

  def describe: String = {
    val actsStr = actFormats.toSeq.map(_.label).sorted.mkString("{", ", ", "}")
    val weisStr = weiFormats.toSeq.map(_.label).sorted.mkString("{", ", ", "}")
    val modeStr = modesSupported.map(m => s"mode${MxPEParams.allModes.indexOf(m)}").mkString(", ")
    val rows = formatSupportTable.map { case (a, w, m) =>
      val idx = MxPEParams.allModes.indexOf(m)
      f"  ${a.label}%-10s x ${w.label}%-10s -> mode$idx"
    }
    val gatingStr = Seq(
      s"fixedNumOutputs=${fixedNumOutputs.getOrElse("runtime")}",
      s"activeOutputLanes=$numActiveOutputLanes",
      s"runtimeMode=$needsRuntimeMode",
      s"runtimeActType=$needsRuntimeActType",
      s"runtimeWeiType=$needsRuntimeWeiType",
      s"out1=${needsOut1}, out2=${needsOut2}, out4=${needsOut4}"
    ).mkString(", ")
    s"""MxConfig: ${modesSupported.length} mode(s), out bus = $outPE_width bits, lane widths = ${laneWidths.mkString("[", ", ", "]")}
       |  actFormats = $actsStr
       |  weiFormats = $weisStr
       |  modes used = $modeStr
       |  gating: $gatingStr
       |  supported operations:
       |${rows.mkString("\n")}""".stripMargin
  }
}

object MxConfig {
  /** Minimum bus width needed for a set of formats.
    * Dual-lane formats (sigWidth < 4) need 2 × bitWidth;
    * single-lane formats (sigWidth >= 4) need 1 × bitWidth.  */
  def minBusWidth(formats: Set[MxFormat]): Int = formats.map { f =>
    if (f.sigWidth >= 4) f.bitWidth else f.bitWidth * 2
  }.max

  def all = MxConfig(MxFormat.all, MxFormat.all,
    inActBusWidth = minBusWidth(MxFormat.all),
    inWeiBusWidth = minBusWidth(MxFormat.all),
    expAdderWidths = Seq(5, 5, 5, 5))
  def fp4Only = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP4))
  def fp6 = MxConfig(MxFormat.fp6, MxFormat.fp6)
  def fp8 = MxConfig(MxFormat.fp8, MxFormat.fp8,
    inActBusWidth = minBusWidth(MxFormat.fp8),
    inWeiBusWidth = minBusWidth(MxFormat.fp8))
  def mxGemmini = MxConfig(
    actFormats = Set(MxFormat.FP4, MxFormat.FP6_E3M2, MxFormat.FP8_E4M3),
    weiFormats = Set(MxFormat.FP4, MxFormat.FP6_E3M2, MxFormat.FP8_E4M3),
    modesOverride = Some(List(MxPEParams.mode0, MxPEParams.mode4, MxPEParams.mode8)),
  )
}

// -----------------------------------------------------------------------------
// MX FLOAT BUNDLE
// -----------------------------------------------------------------------------

case class MxFloat(format: MxFormat, count: Int, isRecoded: Boolean = false, pad: Boolean = true) extends Bundle {
  val expWidth: Int = format.expWidth
  val sigWidth: Int = format.sigWidth
  val bits = if (pad) {
    UInt((1<<log2Ceil(count * (expWidth + sigWidth + (if (isRecoded) 1 else 0)))).W)
  } else {
    UInt((count * (expWidth + sigWidth + (if (isRecoded) 1 else 0))).W)
  }
  val bias: Int = format.bias
}

object MxFloat {
  def apply(expWidth: Int, sigWidth: Int, count: Int, isRecoded: Boolean, pad: Boolean): MxFloat =
    MxFloat(MxFormat(expWidth, sigWidth), count, isRecoded, pad)
  def apply(expWidth: Int, sigWidth: Int, count: Int): MxFloat =
    MxFloat(MxFormat(expWidth, sigWidth), count)
}
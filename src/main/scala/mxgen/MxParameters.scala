package mxgen

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// NAMED MX FORMATS (user-facing enum of supported floating-point formats)
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
  // mode9 (M1): E4M3×E4M3 at 4 elements/cycle via the LUT path. 2 act × 2 wei -> 4 products, each a
  // full 4×4 (4 MACUs) -> needs the 16-MACU array (M2). Shift is the mode8 4×4 pattern, shared by all
  // 4 lanes. Selected only when lut_en is set (requiredPEMode); inert otherwise.
  def mode9 = MxPEParams().copy(
    actTotalWidth = 8,
    weiTotalWidth = 8,
    actWidth = 4,
    weiWidth = 4,
    weiInputs = 2,
    actInputs = 2,
    shift = Seq(Seq(4,2), Seq(2,0)),
    outTotalWidth = 32,
    numOutputs = 4
  )
  // mode10 (M1): E4M3-quad activation x small weight (fp4 or e3m2), 2 act x 2 wei -> 4 products via the
  // LaneMul asymmetric 4xN path. weiWidth=3 sizes the small side for e3m2; the 3-bit M2 add is inert for
  // fp4 (its 3rd sig bit is always 0), so this one mode covers both (4,2) and (4,3). lut_en only.
  def mode10 = MxPEParams().copy(
    actTotalWidth = 8,
    weiTotalWidth = 6,
    actWidth = 4,
    weiWidth = 3,
    weiInputs = 2,
    actInputs = 2,
    shift = Seq(Seq(4,2), Seq(2,0)),
    outTotalWidth = 32,   // 8-bit lanes: LaneMul.io.out is 8b, so lane width must be 8 (product uses low <=7)
    numOutputs = 4
  )
  // mode11 (M1): small activation (fp4 or e3m2) x E4M3-quad weight -- mirror of mode10 (E4M3 is the weight).
  // actWidth=3 sizes the small side for e3m2, inert for fp4; covers both (2,4) and (3,4). lut_en only.
  def mode11 = MxPEParams().copy(
    actTotalWidth = 6,
    weiTotalWidth = 8,
    actWidth = 3,
    weiWidth = 4,
    weiInputs = 2,
    actInputs = 2,
    shift = Seq(Seq(4,2), Seq(2,0)),
    outTotalWidth = 32,
    numOutputs = 4
  )
  val allModes: List[MxPEParams] = List(mode0, mode1, mode2, mode3, mode4, mode5, mode6, mode7, mode8, mode9, mode10, mode11)
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

// TOP-LEVEL CONFIG — user-facing config for MxPE and MxFpMul.
// Bus/adder/lane widths default from the format sets but can be overridden.
case class MxConfig (
  actFormats:       Set[MxFormat]              = Set(MxFormat.FP4),
  weiFormats:       Set[MxFormat]              = Set(MxFormat.FP4),
  productFormat:    MxFormat                   = MxFormat.Custom(8, 8),
  accFormat:        MxFormat                   = MxFormat.Custom(8, 8),
  inActBusWidth:    Int                        = 12,
  inWeiBusWidth:    Int                        = 12,
  expAdderWidths:   Seq[Int]                   = Seq(4, 3, 3, 3),
  laneOutputWidths: Option[Seq[Int]]           = None,
  modesOverride:    Option[List[MxPEParams]]   = None,
  // When true, swap the default add chain for an open-source BF16 adder
  // (cvfpu/fpnew). The MxPE multiplier frontend is preserved; the PE product
  // is rounded to BF16 before the add. Requires accFormat==BF16(8,8).
  useFpnewAdder:    Boolean                    = false,
  // When true, use the bespoke per-lane MxPEAddRecFN. Currently validated for
  // accFormat=BF16; narrower accFormat may produce wrong results. Left
  // accessible for experimentation but not the default.
  useMxPEAddRecFN:  Boolean                    = false,
  // MxDotProduct only: lower bound on bits of headroom above max-exp when
  // setting the anchor. Auto-bumped to fit sum-overflow if too small.
  anchorHeadroom:   Int                        = 4,
) {
  require(actFormats.nonEmpty, "MxConfig: actFormats must not be empty")
  require(weiFormats.nonEmpty, "MxConfig: weiFormats must not be empty")
  require(expAdderWidths.length == 4, "MxConfig.expAdderWidths must have 4 entries (one per lane)")
  laneOutputWidths.foreach(s => require(s.length == 4, "MxConfig.laneOutputWidths must have 4 entries"))
  require(inActBusWidth >= MxConfig.minBusWidth(actFormats),
    s"MxConfig: inActBusWidth=$inActBusWidth too narrow for actFormats (need >= ${MxConfig.minBusWidth(actFormats)})")
  require(inWeiBusWidth >= MxConfig.minBusWidth(weiFormats),
    s"MxConfig: inWeiBusWidth=$inWeiBusWidth too narrow for weiFormats (need >= ${MxConfig.minBusWidth(weiFormats)})")

  // PE mode slots: modesOverride, or cartesian product of actFormats×weiFormats.
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

  // Elaboration-time gating flags — let MxPE/MxFpMul skip hardware that is
  // unreachable for the configured format/mode set.
  val numOutputsValues: Set[Int] = modesSupported.map(_.numOutputs).toSet
  val fixedNumOutputs: Option[Int] = if (numOutputsValues.size == 1) Some(numOutputsValues.head) else None
  val needsOut1: Boolean = numOutputsValues.contains(1)
  val needsOut2: Boolean = numOutputsValues.contains(2)
  val needsOut4: Boolean = numOutputsValues.contains(4)
  val numActiveOutputLanes: Int = numOutputsValues.max

  // Any quad mode with a 4-bit operand (mode9 E4M3xE4M3, or the mixed mode10/mode11 E4M3-quad x small) ->
  // use the LaneMul quad datapath and treat the 4-bit format as dual (2-element). Absent -> the compact
  // 4-MACU MxPE, byte-identical to before.
  val hasMode9: Boolean = modesSupported.exists(m =>
    m.numOutputs == 4 && (m.actWidth == 4 || m.weiWidth == 4))

  // Decoded exponent-bus width, decoupled from the raw operand storage width. Max of the historical
  // (inBusWidth - inPE_totalWidth) -- preserves existing configs -- and what the formats need: dual =
  // 2*maxExp of dual formats, single = maxExp of single formats. Lets E5M2 (exp5) coexist with mode9.
  private def dualExpNeed(fs: Set[MxFormat]): Int = {
    val dual = if (hasMode9) fs else fs.filter(_.sigWidth < 4)
    if (dual.isEmpty) 0 else 2 * dual.map(_.expWidth).max
  }
  private def sngExpNeed(fs: Set[MxFormat]): Int = {
    val sng = if (hasMode9) Set.empty[MxFormat] else fs.filter(_.sigWidth >= 4)
    if (sng.isEmpty) 0 else sng.map(_.expWidth).max
  }
  val inActExpBusWidth: Int = Seq(inActBusWidth - inPE_act_totalWidth, dualExpNeed(actFormats), sngExpNeed(actFormats)).max
  val inWeiExpBusWidth: Int = Seq(inWeiBusWidth - inPE_wei_totalWidth, dualExpNeed(weiFormats), sngExpNeed(weiFormats)).max

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
  /** Min bus width for a set of formats: sig<4 needs 2×bitWidth, else 1×. */
  def minBusWidth(formats: Set[MxFormat]): Int = formats.map { f =>
    if (f.sigWidth >= 4) f.bitWidth else f.bitWidth * 2
  }.max

  def all = MxConfig(MxFormat.all, MxFormat.all,
    inActBusWidth = minBusWidth(MxFormat.all),
    inWeiBusWidth = minBusWidth(MxFormat.all),
    expAdderWidths = Seq(5, 5, 5, 5))
  def fp4Only = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP4),
    expAdderWidths = Seq(3, 3, 3, 3), 
    // productFormat = MxFormat.Custom(3, 3),
    // accFormat = MxFormat.Custom(6, 6)
    )
  def fp6 = MxConfig(MxFormat.fp6, MxFormat.fp6)
  def fp8 = MxConfig(MxFormat.fp8, MxFormat.fp8,
    inActBusWidth = minBusWidth(MxFormat.fp8),
    inWeiBusWidth = minBusWidth(MxFormat.fp8))
  def mxGemmini = MxConfig(
    actFormats = Set(MxFormat.FP4, MxFormat.FP6_E3M2, MxFormat.FP8_E4M3),
    weiFormats = Set(MxFormat.FP4, MxFormat.FP6_E3M2, MxFormat.FP8_E4M3),
    productFormat = MxFormat.Custom(4, 4),
    modesOverride = Some(List(MxPEParams.mode0, MxPEParams.mode4, MxPEParams.mode8)),
  )
  // E5M2 variant: adds FP8_E5M2 (sig=3 -> existing mode4), widens the bus to E5M2's 16b minimum, and
  // widens expAdderWidths for the 5-bit exp.
  def mxGemminiE5M2 = {
    val act = mxGemmini.actFormats + MxFormat.FP8_E5M2
    val wei = mxGemmini.weiFormats + MxFormat.FP8_E5M2
    mxGemmini.copy(
      actFormats = act,
      weiFormats = wei,
      inActBusWidth = minBusWidth(act),
      inWeiBusWidth = minBusWidth(wei),
      expAdderWidths = Seq(5, 5, 5, 5),
    )
  }
  // All-formats config: every MX format coexists {FP4, E3M2, E2M3, E4M3, E5M2}, modes {0,4,8,9}.
  // mode9 -> sig4 formats (E4M3/E2M3) run 4-wide via the 16-MACU quad PE; E5M2 (exp5) rides mode4 with
  // expAdderWidths=5. The 5-bit exp slots fit thanks to inActExpBusWidth decoupling exp from bus width.
  def mxGemminiAll = mxGemmini.copy(
    actFormats     = mxGemmini.actFormats + MxFormat.FP6_E2M3 + MxFormat.FP8_E5M2,
    weiFormats     = mxGemmini.weiFormats + MxFormat.FP6_E2M3 + MxFormat.FP8_E5M2,
    modesOverride  = Some(List(MxPEParams.mode0, MxPEParams.mode4, MxPEParams.mode8, MxPEParams.mode9)),
    inActBusWidth  = 16,
    inWeiBusWidth  = 16,
    expAdderWidths = Seq(5, 5, 5, 5),
  )

  // Single-format builds: the PE elaborates ONLY that format's decode + mode(s); everything else gated.
  // modesSupported auto-derives from the format (FP4->mode0, E3M2/E5M2->mode4); the mode9 quad formats
  // (E2M3, E4M3) list modes explicitly. Bus width comes from the operand descriptor at mac_mx.
  def e3m2Only = MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP6_E3M2),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 12, inWeiBusWidth = 12, expAdderWidths = Seq(3, 3, 3, 3))
  def e2m3Only = MxConfig(Set(MxFormat.FP6_E2M3), Set(MxFormat.FP6_E2M3),
    productFormat = MxFormat.Custom(4, 4),
    modesOverride = Some(List(MxPEParams.mode8, MxPEParams.mode9)),
    inActBusWidth = 12, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
  def e4m3Only = MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP8_E4M3),
    productFormat = MxFormat.Custom(4, 4),
    modesOverride = Some(List(MxPEParams.mode8, MxPEParams.mode9)),
    inActBusWidth = 16, inWeiBusWidth = 16, expAdderWidths = Seq(4, 4, 4, 4))
  def e5m2Only = MxConfig(Set(MxFormat.FP8_E5M2), Set(MxFormat.FP8_E5M2),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 16, inWeiBusWidth = 16, expAdderWidths = Seq(5, 5, 5, 5))

  // Asymmetric build: FP4 activation x FP6_E3M2 weight -> mode1 (4 products/PE). actFormats/weiFormats
  // differ, so modesSupported derives to just [mode1]; the act bus stays fp4-narrow.
  def asymFp4Fp6 = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP6_E3M2),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 8, inWeiBusWidth = 12, expAdderWidths = Seq(3, 3, 3, 3))

  // Opposite asymmetric build: FP6_E3M2 activation x FP4 weight -> mode3. Weight bus stays fp4-narrow.
  def asymFp6Fp4 = MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP4),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 12, inWeiBusWidth = 8, expAdderWidths = Seq(3, 3, 3, 3))

  // Asymmetric build: FP8_E5M2 activation x FP4 weight -> mode3 (sig3 x sig2). Same 4-product bandwidth;
  // E5M2's exp5 needs the 5-bit exp adders, act bus is E5M2's 16b minimum.
  def asymE5M2Fp4 = MxConfig(Set(MxFormat.FP8_E5M2), Set(MxFormat.FP4),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 16, inWeiBusWidth = 8, expAdderWidths = Seq(5, 5, 5, 5))

  // Opposite: FP4 activation x FP8_E5M2 weight -> mode1. E5M2 weight bus is its 16b minimum.
  def asymFp4E5M2 = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP8_E5M2),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 8, inWeiBusWidth = 16, expAdderWidths = Seq(5, 5, 5, 5))

  // FP8_E5M2 activation x FP6_E3M2 weight -> mode4 (both sig3, both LUT-deprojected). Per-operand altfmt
  // (E5M2 alt1, E3M2 alt0) + per-operand deproject width (act 8b, wei 6b).
  def asymE5M2E3M2 = MxConfig(Set(MxFormat.FP8_E5M2), Set(MxFormat.FP6_E3M2),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 16, inWeiBusWidth = 12, expAdderWidths = Seq(5, 5, 5, 5))

  // Opposite: FP6_E3M2 activation x FP8_E5M2 weight -> mode4 (act 6b, wei 8b).
  def asymE3M2E5M2 = MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP8_E5M2),
    productFormat = MxFormat.Custom(4, 4),
    inActBusWidth = 12, inWeiBusWidth = 16, expAdderWidths = Seq(5, 5, 5, 5))

  // FP8_E4M3 activation x FP4 weight: mode10 (E4M3 quad, lut on, 4 products) or mode6 (E4M3 single, lut off, 2).
  def asymE4M3Fp4 = MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP4),
    productFormat = MxFormat.Custom(4, 4),
    modesOverride = Some(List(MxPEParams.mode6, MxPEParams.mode10)),
    inActBusWidth = 16, inWeiBusWidth = 8, expAdderWidths = Seq(4, 4, 4, 4))

  // FP4 activation x FP8_E4M3 weight: mode11 (E4M3 quad, lut on, 4 products) or mode2 (E4M3 single, lut off, 2).
  def asymFp4E4M3 = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP8_E4M3),
    productFormat = MxFormat.Custom(4, 4),
    modesOverride = Some(List(MxPEParams.mode2, MxPEParams.mode11)),
    inActBusWidth = 8, inWeiBusWidth = 16, expAdderWidths = Seq(4, 4, 4, 4))

  // E4M3-quad x sig3 (both LUT-deprojected). mode10 (E4M3 act) / mode11 (E4M3 wei); weiWidth/actWidth=3
  // covers e3m2 and e5m2. E5M2 pairs need per-operand altfmt (both fp8 code0) + exp5 adders.
  def asymE4M3E3M2 = MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP6_E3M2),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode7, MxPEParams.mode10)),
    inActBusWidth = 16, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE4M3E5M2 = MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP8_E5M2),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode10)),
    inActBusWidth = 16, inWeiBusWidth = 16, expAdderWidths = Seq(5, 5, 5, 5))
  def asymE3M2E4M3 = MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP8_E4M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode5, MxPEParams.mode11)),
    inActBusWidth = 12, inWeiBusWidth = 16, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE5M2E4M3 = MxConfig(Set(MxFormat.FP8_E5M2), Set(MxFormat.FP8_E4M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode11)),
    inActBusWidth = 16, inWeiBusWidth = 16, expAdderWidths = Seq(5, 5, 5, 5))

  // E2M3 (sig4, exp2, fp6/LUT) mixed-quad combos -- same mode10/mode11 slots as E4M3 (sig-keyed).
  def asymE2M3Fp4 = MxConfig(Set(MxFormat.FP6_E2M3), Set(MxFormat.FP4),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode10)),
    inActBusWidth = 12, inWeiBusWidth = 8, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE2M3E3M2 = MxConfig(Set(MxFormat.FP6_E2M3), Set(MxFormat.FP6_E3M2),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode10)),
    inActBusWidth = 12, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE2M3E5M2 = MxConfig(Set(MxFormat.FP6_E2M3), Set(MxFormat.FP8_E5M2),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode10)),
    inActBusWidth = 12, inWeiBusWidth = 16, expAdderWidths = Seq(5, 5, 5, 5))
  def asymFp4E2M3 = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP6_E2M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode11)),
    inActBusWidth = 8, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE3M2E2M3 = MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP6_E2M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode11)),
    inActBusWidth = 12, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE5M2E2M3 = MxConfig(Set(MxFormat.FP8_E5M2), Set(MxFormat.FP6_E2M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode11)),
    inActBusWidth = 16, inWeiBusWidth = 12, expAdderWidths = Seq(5, 5, 5, 5))

  // Dual-sig4 quad: E2M3 x E4M3 (both sig4) -> mode9, the true 2/lane x 2/lane = 4-product quad. Mixed
  // code widths (E2M3 6b, E4M3 8b) + per-operand altfmt (E2M3 fp6-alt1, E4M3 fp8-alt0).
  def asymE2M3E4M3 = MxConfig(Set(MxFormat.FP6_E2M3), Set(MxFormat.FP8_E4M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode9)),
    inActBusWidth = 12, inWeiBusWidth = 16, expAdderWidths = Seq(4, 4, 4, 4))
  def asymE4M3E2M3 = MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP6_E2M3),
    productFormat = MxFormat.Custom(4, 4), modesOverride = Some(List(MxPEParams.mode9)),
    inActBusWidth = 16, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
}

// MX FLOAT BUNDLE
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
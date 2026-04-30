package mxgen

import chisel3._
import chisel3.util._
import hardfloat._
import mxgen.hardfloat.{MxHardfloatFMA, MxPerFormatFMA}

// Which DUT to wrap in the testbench. The three modules share an identical
// IO bundle but live in different classes.
sealed trait MxPEDutKind { def label: String }
object MxPEDutKind {
  case class MxFpMulDut(lut: Boolean = false) extends MxPEDutKind {
    val label = if (lut) "mxfpmul-lut" else "mxfpmul"
  }
  case object HardfloatFMADut extends MxPEDutKind { val label = "hardfloat" }
  case object PerFormatFMADut extends MxPEDutKind { val label = "performat" }
}

/** A precomputed test vector: raw operand bits as the DUT sees them, and the
  * expected per-lane BF16 output (BF16 raw, 16b each), with a 4-bit valid
  * mask flagging which lanes are checked. Lane order: lane0 in [15:0]. */
case class MxPETBVector(
  inAct: BigInt,
  inWei: BigInt,
  typeAExp: Int, typeASig: Int,
  typeWExp: Int, typeWSig: Int,
  cBf16: Int,
  expLane0: Int, expLane1: Int, expLane2: Int, expLane3: Int,
  validMask: Int
)

object MxPETBVectors {

  // ---------- mini-format helpers (mirror test/MxPETestBase) ----------
  case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
    val expMask  = (1 << eBits) - 1
    val mantMask = (1 << mBits) - 1
    def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask)
  }
  val FP4_E2M1 = MiniFmt(2, 1, bias = 1)
  val FP6_E2M3 = MiniFmt(2, 3, bias = 1)
  val FP6_E3M2 = MiniFmt(3, 2, bias = 3)
  val FP8_E4M3 = MiniFmt(4, 3, bias = 7)
  val FP8_E5M2 = MiniFmt(5, 2, bias = 15)

  case class FormatDesc(name: String, typeCode: Int, alt: Boolean, fmt: MiniFmt) {
    def sigWidth: Int = fmt.mBits + 1
    def isDual: Boolean = sigWidth < 4
    def expWidth: Int = fmt.eBits
  }
  val descByMxFormat: Map[MxFormat, FormatDesc] = Map(
    MxFormat.FP4      -> FormatDesc("fp4",  0, false, FP4_E2M1),
    MxFormat.FP6_E2M3 -> FormatDesc("e2m3", 1, false, FP6_E2M3),
    MxFormat.FP6_E3M2 -> FormatDesc("e3m2", 1, true,  FP6_E3M2),
    MxFormat.FP8_E4M3 -> FormatDesc("e4m3", 2, false, FP8_E4M3),
    MxFormat.FP8_E5M2 -> FormatDesc("e5m2", 2, true,  FP8_E5M2)
  )

  def decodeSmall(fmt: MiniFmt, raw: Int): Float = {
    val e = (raw >> fmt.mBits) & fmt.expMask
    val m = raw & fmt.mantMask
    if (e == 0) {
      if (m == 0) 0.0f
      else (m.toFloat / (1 << fmt.mBits).toFloat) * math.pow(2.0, 1 - fmt.bias).toFloat
    } else {
      val frac = 1.0f + m.toFloat / (1 << fmt.mBits).toFloat
      (frac * math.pow(2.0, e - fmt.bias)).toFloat
    }
  }

  def bf16ToFloat(raw16: Int): Float = java.lang.Float.intBitsToFloat(raw16 << 16)
  def floatToBf16Raw(f: Float): Int = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lsb  = (bits >>> 16) & 1
    val rnd  = bits + (0x7FFF + lsb)
    (rnd >>> 16) & 0xFFFF
  }

  def genSmall(rng: scala.util.Random, fmt: MiniFmt): Int = {
    val r = rng.nextFloat()
    if (r < 0.10f) fmt.enc(0, 0)
    else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask max 1))
    else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1),
                 rng.nextInt(fmt.mantMask + 1))
  }
  def genBF16(rng: scala.util.Random): Int = {
    val r = rng.nextFloat()
    if (r < 0.10f) 0x0000
    else if (r < 0.20f) 0x0001 + rng.nextInt(0x7F)
    else {
      val mag  = math.pow(2.0, rng.nextInt(8) - 4).toFloat
      val base = rng.nextFloat() * mag
      floatToBf16Raw(base)
    }
  }

  def pack2IntoHalves(raw0: Int, raw1: Int, elemBits: Int, totalW: Int): BigInt = {
    val half = totalW / 2
    val m    = (1 << elemBits) - 1
    BigInt(raw0 & m) | (BigInt(raw1 & m) << half)
  }

  // Pack operand raw bits into the DUT's bus exactly the same way the tests do.
  // The wei side has identical packing rules to the act side, so packOp covers both.
  def packOp(typeCode: Int, alt: Boolean, raws: Seq[Int], busW: Int): (BigInt, Seq[Float]) =
    typeCode match {
      case 0 =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), 4, busW)
        (packed, raws.map(r => decodeSmall(FP4_E2M1, r & 0xF)))
      case 1 if alt =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), 6, busW)
        (packed, raws.map(r => decodeSmall(FP6_E3M2, r & 0x3F)))
      case 1 =>
        require(raws.length == 1)
        val v = raws.head & 0x3F
        (BigInt(v), Seq(decodeSmall(FP6_E2M3, v)))
      case 2 if alt =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), 8, busW)
        (packed, raws.map(r => decodeSmall(FP8_E5M2, r & 0xFF)))
      case 2 =>
        require(raws.length == 1)
        val v = raws.head & 0xFF
        (BigInt(v), Seq(decodeSmall(FP8_E4M3, v)))
    }

  def expectedBF16Lane(aVals: Seq[Float], wVals: Seq[Float], c: Float, lane: Int): Option[Int] = {
    def toBF16(x: Float) = floatToBf16Raw(x)
    (aVals.length, wVals.length) match {
      case (2, 2) =>
        val pairs = Array((0,0),(0,1),(1,0),(1,1))
        val (ai, wi) = pairs(lane)
        Some(toBF16(aVals(ai) * wVals(wi) + c))
      case (2, 1) => lane match {
        case 0 | 1 => Some(toBF16(aVals(0) * wVals(0) + c))
        case 2 | 3 => Some(toBF16(aVals(1) * wVals(0) + c))
        case _     => None
      }
      case (1, 2) => lane match {
        case 0 | 1 => Some(toBF16(aVals(0) * wVals(0) + c))
        case 2 | 3 => Some(toBF16(aVals(0) * wVals(1) + c))
        case _     => None
      }
      case (1, 1) => if (lane == 0) Some(toBF16(aVals(0) * wVals(0) + c)) else None
      case _ => None
    }
  }

  /** Generate test vectors for `config`, optionally restricting to same-format
    * pairs (used for MxPerFormatFMA). Vectors are clamped to `maxVectors`. */
  def generate(
    config:         MxConfig,
    sameFormatOnly: Boolean,
    maxVectors:     Int    = 30,
    seed:           Long   = 0xBEEFBABEL,
  ): Seq[MxPETBVector] = {
    val rng = new scala.util.Random(seed)

    val acts = config.actFormats.toSeq.sortBy(_.label).map(descByMxFormat)
    val weis = config.weiFormats.toSeq.sortBy(_.label).map(descByMxFormat)
    val supportedSigPairs: Set[(Int, Int)] = config.sigWidthPairs

    val combos: Seq[(FormatDesc, FormatDesc)] = for {
      a <- acts
      w <- weis
      if !sameFormatOnly || (a.typeCode == w.typeCode && a.alt == w.alt)
      if supportedSigPairs.contains((a.sigWidth, w.sigWidth))
    } yield (a, w)

    require(combos.nonEmpty, s"MxPETBVectors.generate: no supported combos for config (sameFormatOnly=$sameFormatOnly)")

    // Aim for ~24 vectors per TB; cap per-combo at 8, take at most maxVectors.
    val nCombos        = combos.length
    val trialsPerCombo = math.max(2, math.min(8, math.ceil(24.0 / nCombos).toInt))

    val aW = config.inActBusWidth
    val wW = config.inWeiBusWidth
    val numActive = config.numActiveOutputLanes

    val all: Seq[MxPETBVector] = combos.flatMap { case (a, w) =>
      (0 until trialsPerCombo).map { _ =>
        val aRaws = Seq.fill(if (a.isDual) 2 else 1)(genSmall(rng, a.fmt))
        val wRaws = Seq.fill(if (w.isDual) 2 else 1)(genSmall(rng, w.fmt))
        val (aPacked, aVals) = packOp(a.typeCode, a.alt, aRaws, aW)
        val (wPacked, wVals) = packOp(w.typeCode, w.alt, wRaws, wW)
        val cRaw = genBF16(rng)
        val cVal = bf16ToFloat(cRaw)

        val laneOpts  = (0 until 4).map(i => expectedBF16Lane(aVals, wVals, cVal, i))
        val expLanes  = laneOpts.map(_.getOrElse(0))
        val validMask = (0 until numActive).map(i =>
          if (laneOpts(i).isDefined) 1 << i else 0
        ).sum

        MxPETBVector(
          inAct = aPacked, inWei = wPacked,
          typeAExp = a.expWidth, typeASig = a.sigWidth,
          typeWExp = w.expWidth, typeWSig = w.sigWidth,
          cBf16 = cRaw,
          expLane0 = expLanes(0), expLane1 = expLanes(1),
          expLane2 = expLanes(2), expLane3 = expLanes(3),
          validMask = validMask
        )
      }
    }
    if (all.length <= maxVectors) all else all.take(maxVectors)
  }
}

/** Self-checking, ROM-driven Chisel testbench for one of the three Mx PE
  * designs. Compiles to plain SystemVerilog: no chiseltest, no $readmemh.
  *
  *  - inputs come from a VecInit ROM of precomputed vectors
  *  - BF16 c-in is converted to recFN(accFormat) in hardware
  *  - lane outputs are converted back to BF16 raw in hardware
  *  - lanes are compared against per-vector expected values, masked to
  *    `numActiveOutputLanes`; mismatches increment `numFails` and emit
  *    a `printf` (rendered as `$write` in SV simulation).
  *
  * Drives vec[idx] for `latency+1` cycles, samples on the last cycle.
  * Total runtime = `numTests * (latency+1)` cycles after reset.
  */
class MxPEVerilogTB(
  config:        MxConfig,
  kind:          MxPEDutKind,
  vectors:       Seq[MxPETBVector],
  latency:       Int    = 0,
  override val desiredName: String = "MxPEVerilogTB"
) extends Module {
  require(latency >= 0 && latency <= 2,
    s"MxPEVerilogTB: latency must be 0, 1, or 2 (got $latency)")
  require(vectors.nonEmpty, "MxPEVerilogTB: vectors must be non-empty")

  val numTests  = vectors.length
  val numActive = config.numActiveOutputLanes
  val accExp    = config.accFormat.exp
  val accSig    = config.accFormat.sig
  val accRecW   = config.accFormat.recoded

  val io = IO(new Bundle {
    val done        = Output(Bool())
    val pass        = Output(Bool())
    val numFails    = Output(UInt(32.W))
    val testIdx     = Output(UInt(32.W))
    val curObserved = Output(UInt(64.W))
    val curExpected = Output(UInt(64.W))
  })

  // ---------- ROMs ----------
  val inActVec    = VecInit(vectors.map(v => v.inAct.U(config.inActBusWidth.W)))
  val inWeiVec    = VecInit(vectors.map(v => v.inWei.U(config.inWeiBusWidth.W)))
  val typeAExpVec = VecInit(vectors.map(v => v.typeAExp.U(3.W)))
  val typeASigVec = VecInit(vectors.map(v => v.typeASig.U(3.W)))
  val typeWExpVec = VecInit(vectors.map(v => v.typeWExp.U(3.W)))
  val typeWSigVec = VecInit(vectors.map(v => v.typeWSig.U(3.W)))
  val cBf16Vec    = VecInit(vectors.map(v => v.cBf16.U(16.W)))
  val expVec      = VecInit(vectors.map { v =>
    val packed = (BigInt(v.expLane3 & 0xFFFF) << 48) |
                 (BigInt(v.expLane2 & 0xFFFF) << 32) |
                 (BigInt(v.expLane1 & 0xFFFF) << 16) |
                  BigInt(v.expLane0 & 0xFFFF)
    packed.U(64.W)
  })
  val maskVec = VecInit(vectors.map(v => v.validMask.U(4.W)))

  // ---------- counters ----------
  val idxW   = log2Ceil(numTests + 1)
  val phaseW = log2Ceil(latency + 2)
  val idx   = RegInit(0.U(idxW.W))
  val phase = RegInit(0.U(phaseW.W))
  val done  = RegInit(false.B)
  val fails = RegInit(0.U(32.W))

  // ---------- inputs ----------
  val typeA = Wire(new MxTypeBundle); typeA.exp := typeAExpVec(idx); typeA.sig := typeASigVec(idx)
  val typeW = Wire(new MxTypeBundle); typeW.exp := typeWExpVec(idx); typeW.sig := typeWSigVec(idx)
  val mode  = requiredPEMode(typeA, typeW)

  // BF16 c -> recFN(accFormat); broadcast across active lanes.
  val recCBf16 = hardfloat.recFNFromFN(8, 8, cBf16Vec(idx))
  val recCAcc: UInt = if (accExp == 8 && accSig == 8) {
    recCBf16
  } else {
    val rawC  = rawFloatFromRecFN(8, 8, recCBf16)
    val cConv = Module(new RoundAnyRawFNToRecFN(8, 8, accExp, accSig, 0))
    cConv.io.in             := rawC
    cConv.io.roundingMode   := hardfloat.consts.round_near_even
    cConv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    cConv.io.invalidExc     := false.B
    cConv.io.infiniteExc    := false.B
    cConv.io.out
  }
  val recCBroadcast = Cat(Seq.fill(numActive)(recCAcc).reverse)

  // ---------- DUT ----------
  val dutOut: UInt = kind match {
    case MxPEDutKind.MxFpMulDut(lut) =>
      val d = Module(new MxFpMul(config, lut, latency = latency))
      d.io.in_activation := inActVec(idx)
      d.io.in_weights    := inWeiVec(idx)
      d.io.type_a        := typeA
      d.io.type_w        := typeW
      d.io.mode          := mode
      d.io.enable        := true.B
      d.io.rec_c         := recCBroadcast
      d.io.out
    case MxPEDutKind.HardfloatFMADut =>
      val d = Module(new MxHardfloatFMA(config, latency = latency))
      d.io.in_activation := inActVec(idx)
      d.io.in_weights    := inWeiVec(idx)
      d.io.type_a        := typeA
      d.io.type_w        := typeW
      d.io.mode          := mode
      d.io.enable        := true.B
      d.io.rec_c         := recCBroadcast
      d.io.out
    case MxPEDutKind.PerFormatFMADut =>
      val d = Module(new MxPerFormatFMA(config, latency = latency))
      d.io.in_activation := inActVec(idx)
      d.io.in_weights    := inWeiVec(idx)
      d.io.type_a        := typeA
      d.io.type_w        := typeW
      d.io.mode          := mode
      d.io.enable        := true.B
      d.io.rec_c         := recCBroadcast
      d.io.out
  }

  // ---------- lane outputs -> BF16 raw ----------
  val lanesAcc = Wire(Vec(4, UInt(accRecW.W)))
  for (i <- 0 until 4) {
    if (i < numActive) {
      val hi = (i + 1) * accRecW - 1
      val lo = i * accRecW
      lanesAcc(i) := dutOut(hi, lo)
    } else {
      lanesAcc(i) := 0.U
    }
  }
  val lanesBf16Rec = Wire(Vec(4, UInt(17.W)))
  for (i <- 0 until 4) {
    if (accExp == 8 && accSig == 8) {
      lanesBf16Rec(i) := lanesAcc(i)
    } else {
      val rawLane = rawFloatFromRecFN(accExp, accSig, lanesAcc(i))
      val up      = Module(new RoundAnyRawFNToRecFN(accExp, accSig, 8, 8, 0))
      up.io.in             := rawLane
      up.io.roundingMode   := hardfloat.consts.round_near_even
      up.io.detectTininess := hardfloat.consts.tininess_afterRounding
      up.io.invalidExc     := false.B
      up.io.infiniteExc    := false.B
      lanesBf16Rec(i) := up.io.out
    }
  }
  val lanesBF16 = Wire(Vec(4, UInt(16.W)))
  for (i <- 0 until 4) lanesBF16(i) := hardfloat.fNFromRecFN(8, 8, lanesBf16Rec(i))
  val observedPacked = Cat(lanesBF16.reverse)

  // ---------- compare / advance ----------
  val sampleNow = phase === latency.U
  val expPacked = expVec(idx)
  val mask      = maskVec(idx)

  val laneMatch = VecInit((0 until 4).map { i =>
    val expL = expPacked(i*16+15, i*16)
    val obsL = observedPacked(i*16+15, i*16)
    !mask(i) || (expL === obsL)
  })
  val testPass = laneMatch.asUInt.andR

  when (!done) {
    when (sampleNow) {
      when (!testPass) {
        fails := fails + 1.U
        printf(p"[MxPEVerilogTB ${desiredName}] FAIL idx=$idx exp=0x${Hexadecimal(expPacked)} got=0x${Hexadecimal(observedPacked)} mask=0x${Hexadecimal(mask)}\n")
      }
      when (idx === (numTests - 1).U) {
        done := true.B
        printf(p"[MxPEVerilogTB ${desiredName}] DONE total=${numTests.U} fails=$fails\n")
      } .otherwise {
        idx   := idx + 1.U
        phase := 0.U
      }
    } .otherwise {
      phase := phase + 1.U
    }
  }

  io.done        := done
  io.pass        := done && (fails === 0.U)
  io.numFails    := fails
  io.testIdx     := idx
  io.curObserved := observedPacked
  io.curExpected := expPacked
}

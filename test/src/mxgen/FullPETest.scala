package mxgen

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import hardfloat._

// Shared IO bundle for both MxFpMul and MxHardfloatFMA harnesses. The two DUTs
// have byte-identical IO; exposing a common harness bundle lets the randomized
// test base target either one via `makeHarness`.
class Bf16OutHarnessIOBundle(config: MxConfig) extends Bundle {
  val in_activation = Input(UInt(config.inActBusWidth.W))
  val type_a        = Input(new MxTypeBundle())
  val in_weights    = Input(UInt(config.inWeiBusWidth.W))
  val type_w        = Input(new MxTypeBundle())
  val mode          = Input(new mxMode())
  val enable        = Input(Bool())
  // BF16 raw C (E8M7): 1|8|7 = 16b
  val c_raw         = Input(UInt(16.W))
  // 4 × BF16 packed LSB-first (lane0 in [15:0])
  val out_bf16      = Output(UInt(64.W))
  // Each DUT lane widened to recFN(8,8) (BF16) → 4 × 17 = 68b, lane0 at [16:0].
  val out_recfn     = Output(UInt(68.W))
  // Observe the rec_c vector actually applied to the DUT.
  val rec_c_applied = Output(UInt((config.numActiveOutputLanes * config.accFormat.recoded).W))
}

abstract class Bf16OutHarnessBase(config: MxConfig) extends Module {
  val io: Bf16OutHarnessIOBundle
}

/** Harness that mirrors *new* MxFpMul IO exactly, but:
  *  - Accepts BF16 raw C (E8M7, 16b) and recodes to recFN(8,8) to drive dut.io.rec_c
  *  - Exposes dut.io.out as 4×BF16 raw packed (64b) for easy checking/printing
  */
class MxFpMulHarnessBf16Out_NewIO(config: MxConfig, lut: Boolean)
    extends Bf16OutHarnessBase(config) {

  val dut = Module(new MxFpMul(config, lut))

  val accExp    = config.accFormat.exp
  val accSig    = config.accFormat.sig
  val accRecW   = config.accFormat.recoded
  val numActive = config.numActiveOutputLanes

  val io = IO(new Bf16OutHarnessIOBundle(config))

  val computedMode = requiredPEMode(io.type_a, io.type_w)
  dut.io.mode := computedMode

  dut.io.in_activation := io.in_activation
  dut.io.type_a        := io.type_a
  dut.io.in_weights    := io.in_weights
  dut.io.type_w        := io.type_w
  dut.io.enable        := io.enable

  // -------- C input: BF16 --> recFN(accFormat) --------
  // Convert the 16-bit BF16 scalar to recFN(8,8), then round-convert down/up
  // to recFN(accFormat) using RoundAnyRawFNToRecFN so any accFormat works.
  val recCBf16 = hardfloat.recFNFromFN(8, 8, io.c_raw)  // 17b
  val recCAcc: UInt = if (accExp == 8 && accSig == 8) {
    recCBf16
  } else {
    val rawC = rawFloatFromRecFN(8, 8, recCBf16)
    val cConv = Module(new RoundAnyRawFNToRecFN(8, 8, accExp, accSig, 0))
    cConv.io.in             := rawC
    cConv.io.roundingMode   := hardfloat.consts.round_near_even
    cConv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    cConv.io.invalidExc     := false.B
    cConv.io.infiniteExc    := false.B
    cConv.io.out
  }

  // Broadcast the single scalar C to every active DUT lane.
  dut.io.rec_c := Cat(Seq.fill(numActive)(recCAcc).reverse)
  io.rec_c_applied := dut.io.rec_c

  // -------- Output lanes: recFN(accFormat) --> recFN(8,8) (widen up to BF16) --------
  require(dut.io.out.getWidth == numActive * accRecW,
    s"DUT out width ${dut.io.out.getWidth} != numActiveOutputLanes*$accRecW")

  val lanesAcc = Wire(Vec(4, UInt(accRecW.W)))
  for (i <- 0 until 4) {
    if (i < numActive) {
      val hi = (i + 1) * accRecW - 1
      val lo = i * accRecW
      lanesAcc(i) := dut.io.out(hi, lo)
    } else {
      lanesAcc(i) := 0.U
    }
  }

  // Widen each lane from recFN(accFormat) up to recFN(8,8). If accFormat is
  // already BF16, the resize is a no-op and we skip instantiating the module.
  val lanesBf16Rec = Wire(Vec(4, UInt(17.W)))
  for (i <- 0 until 4) {
    if (accExp == 8 && accSig == 8) {
      lanesBf16Rec(i) := lanesAcc(i)
    } else {
      val rawLane = rawFloatFromRecFN(accExp, accSig, lanesAcc(i))
      val up = Module(new RoundAnyRawFNToRecFN(accExp, accSig, 8, 8, 0))
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

  io.out_bf16  := Cat(lanesBF16.reverse)    // lane0 at [15:0]
  io.out_recfn := Cat(lanesBf16Rec.reverse) // lane0 at [16:0]
}

class MxFpMul_AllATypes_BF16Out_SelfChecking_NewIO_Spec
  extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  behavior of "MxFpMul — NEW IO; BF16 IEEE output; FP4/FP6=2x2->4 lanes; FP8=1x1->lane0; self-checking"

  it should "run randomized combos and self-check lanes (verbose pre/post prints)" in {

    // MxConfig.mxGemmini supports FP4/FP6_E3M2/FP8_E4M3 (modes 0, 4, 8).
    // Override product/acc to Custom(8,8) (BF16-like) to match old test precision.
    val config = MxConfig.all.copy(
      productFormat = MxFormat(8, 8),
      accFormat = MxFormat(8, 8)
    )

    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

        // ---------- small-format helpers (positive-only) ----------
        case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
          val expMask = (1 << eBits) - 1
          val mantMask= (1 << mBits) - 1
          def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask) // sign=0
        }
        val FP4_E2M1 = MiniFmt(2,1, bias=1)
        val FP6_E2M3 = MiniFmt(2,3, bias=1)   // altfmt = 0
        val FP6_E3M2 = MiniFmt(3,2, bias=3)   // altfmt = 1
        val FP8_E4M3 = MiniFmt(4,3, bias=7)   // altfmt = 0
        val FP8_E5M2 = MiniFmt(5,2, bias=15)  // altfmt = 1

        def decodeSmall(fmt: MiniFmt, raw: Int): scala.Float = {
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

        // ---------- BF16 helpers ----------
        def bf16ToFloat(raw16: Int): scala.Float = java.lang.Float.intBitsToFloat(raw16 << 16)
        def floatToBf16Raw(f: scala.Float): Int = {
          val bits = java.lang.Float.floatToRawIntBits(f)
          val lsb  = (bits >>> 16) & 1
          val rnd  = bits + (0x7FFF + lsb)     // RNE to 16 MSBs
          (rnd >>> 16) & 0xFFFF
        }

        // Pretty printers
        def binStr(x: BigInt, w: Int): String = {
          val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
        }
        def lane(bits: BigInt, idx: Int, laneW: Int): Int =
          ((bits >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)).toInt
        def showBF16(tag: String, v: Int): Unit = {
          val s = (v >>> 15) & 1
          val e = (v >>> 7)  & 0xFF
          val f = v & 0x7F
          println(f"$tag: 0x$v%04X  s=$s e=0x$e%02X f=0x$f%02X  (~=${bf16ToFloat(v)}%g)")
        }

        val rng = new Random(0xBEEFBABE)
        def genSmall(fmt: MiniFmt): Int = {
          val r = rng.nextFloat()
          if (r < 0.10f) fmt.enc(0, 0)                                  // +0
          else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask))  // subnormal
          else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1),         // normal
                       rng.nextInt(fmt.mantMask + 1))
        }
        def genBF16(): Int = {
          val r = rng.nextFloat()
          if (r < 0.10f) 0x0000
          else if (r < 0.20f) (0x0001 + rng.nextInt(0x7F))
          else {
            val mag = math.pow(2.0, rng.nextInt(8) - 4).toFloat
            val base= rng.nextFloat() * mag
            floatToBf16Raw(base)
          }
        }

        val aW = h.io.in_activation.getWidth
        val wW = h.io.in_weights.getWidth
        val laneW = 16 // BF16 lanes on the harness output

        def pack2IntoHalves(raw0: Int, raw1: Int, elemBits: Int, totalW: Int): BigInt = {
            val half = totalW / 2
            require(totalW % 2 == 0, s"totalW=$totalW must be even")
            require(elemBits <= half, s"elemBits=$elemBits must fit in half=$half")
            val m = (1 << elemBits) - 1
            (BigInt(raw0 & m)) | (BigInt(raw1 & m) << half)
        }

        // ------------------------------------------------------------------
        // NEW PACK RULES:
        //   FP4: 2 acts (fp4) and 2 weights (fp4) => 4 outputs
        //   FP6: 2 acts (fp6) and 2 weights (fp6) => 4 outputs
        //   FP8: 1 act (fp8) and 1 weight (fp8) => only lane0 defined
        //
        // NOTE: We keep (aType,wType,alt) in the test like before.
        // ------------------------------------------------------------------

        def packActs(aType: Int, aAlt: Boolean, raws: Seq[Int]): (BigInt, Seq[scala.Float], String) = {
            val totalW = aW   // h.io.in_activation.getWidth
            aType match {
                case 0 => // fp4: still 2 values, each placed in its half-slot
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, totalW)
                val vals   = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
                (packed, vals, f"fp4 acts: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 1 if aAlt => // fp6 E3M2: dual-element, 2 values in half-slots (6b each)
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, totalW)
                val vals   = raws.map(r => decodeSmall(FP6_E3M2, r & 0x3F))
                (packed, vals, s"fp6 E3M2 acts: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 1 => // fp6 E2M3: single-element (sigWidth=4), 1 value at bit 0
                require(raws.length == 1)
                val a0     = raws.head & 0x3F
                val packed = BigInt(a0)
                val vals   = Seq(decodeSmall(FP6_E2M3, a0))
                (packed, vals, f"fp6 E2M3 act: 0x$a0%02X -> ${vals.head}")

                case 2 if aAlt => // fp8 E5M2: dual-element (sigWidth=3), 2 values in half-slots (8b each)
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 8, totalW)
                val vals   = raws.map(r => decodeSmall(FP8_E5M2, r & 0xFF))
                (packed, vals, s"fp8 E5M2 acts: ${raws.map(r => f"0x${r & 0xFF}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 2 => // fp8 E4M3: single-element (sigWidth=4), 1 value at bit 0
                require(raws.length == 1)
                val a0     = raws.head & 0xFF
                val packed = BigInt(a0)
                val vals   = Seq(decodeSmall(FP8_E4M3, a0))
                (packed, vals, f"fp8 E4M3 act: 0x$a0%02X -> ${vals.head}")
            }
        }

        def packWeis(wType: Int, wAlt: Boolean, raws: Seq[Int]): (BigInt, Seq[scala.Float], String) = {
            val totalW = wW // h.io.in_weights.getWidth
            wType match {
                case 0 => // fp4 weights: 2 values in half-slots
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, totalW)
                val vals   = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
                (packed, vals, f"fp4 weis: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 1 if wAlt => // fp6 E3M2: dual-element, 2 values in half-slots (6b each)
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, totalW)
                val vals   = raws.map(r => decodeSmall(FP6_E3M2, r & 0x3F))
                (packed, vals, s"fp6 E3M2 weis: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 1 => // fp6 E2M3: single-element (sigWidth=4), 1 value at bit 0
                require(raws.length == 1)
                val w0     = raws.head & 0x3F
                val packed = BigInt(w0)
                val vals   = Seq(decodeSmall(FP6_E2M3, w0))
                (packed, vals, f"fp6 E2M3 wei: 0x$w0%02X -> ${vals.head}")

                case 2 if wAlt => // fp8 E5M2: dual-element (sigWidth=3), 2 values in half-slots (8b each)
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 8, totalW)
                val vals   = raws.map(r => decodeSmall(FP8_E5M2, r & 0xFF))
                (packed, vals, s"fp8 E5M2 weis: ${raws.map(r => f"0x${r & 0xFF}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 2 => // fp8 E4M3: single-element (sigWidth=4), 1 value at bit0
                require(raws.length == 1)
                val w0     = raws.head & 0xFF
                val packed = BigInt(w0)
                val vals   = Seq(decodeSmall(FP8_E4M3, w0))
                (packed, vals, f"fp8 E4M3 wei: 0x$w0%02X -> ${vals.head}")
            }
        }

        // Expected lane mapping under NEW rules
        //  - For 2 acts × 2 weis: lane0=a0*w0, lane1=a0*w1, lane2=a1*w0, lane3=a1*w1
        //  - For 1×1: lane0 only
        def expectedBF16Lane(aVals: Seq[scala.Float], wVals: Seq[scala.Float], c: scala.Float, lane: Int): Option[Int] = {
          def toBF16(x: scala.Float) = floatToBf16Raw(x)
          (aVals.length, wVals.length) match {
            case (2,2) =>
              val pairs = Array((0,0), (0,1), (1,0), (1,1))
              val (ai, wi) = pairs(lane)
              Some(toBF16(aVals(ai) * wVals(wi) + c))

            case (2,1) => // 2 acts × 1 wei → 2 outputs, broadcast to lane pairs
              lane match {
                case 0 | 1 => Some(toBF16(aVals(0) * wVals(0) + c))
                case 2 | 3 => Some(toBF16(aVals(1) * wVals(0) + c))
                case _     => None
              }

            case (1,2) => // 1 act × 2 weis → 2 outputs, broadcast to lane pairs
              lane match {
                case 0 | 1 => Some(toBF16(aVals(0) * wVals(0) + c))
                case 2 | 3 => Some(toBF16(aVals(0) * wVals(1) + c))
                case _     => None
              }

            case (1,1) =>
              if (lane == 0) Some(toBF16(aVals(0) * wVals(0) + c)) else None

            case _ =>
              None
          }
        }

        // Variants: keep same semantic knobs (type + altfmt), but NEW lane counts
        val aVariants = Seq(
          ("A: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
          ("A: 1×fp6 (E2M3)",      1, false, () => Seq(genSmall(FP6_E2M3))),
          ("A: 2×fp6 (E3M2)",     1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2))),
          ("A: 1×fp8 (E4M3)",     2, false, () => Seq(genSmall(FP8_E4M3))),
          ("A: 2×fp8 (E5M2)",     2, true,  () => Seq(genSmall(FP8_E5M2), genSmall(FP8_E5M2)))
        )
        val wVariants = Seq(
          ("W: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
          ("W: 1×fp6 (E2M3)",      1, false, () => Seq(genSmall(FP6_E2M3))),
          ("W: 2×fp6 (E3M2)",     1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2))),
          ("W: 1×fp8 (E4M3)",     2, false, () => Seq(genSmall(FP8_E4M3))),
          ("W: 2×fp8 (E5M2)",     2, true,  () => Seq(genSmall(FP8_E5M2), genSmall(FP8_E5M2)))
        )

        val trialsPerCombo = 10
        h.io.enable.poke(true.B)

        // ------------------------------------------------------------------
        // TODO: map (aType,aAlt,wType,wAlt) into YOUR actual Bundle fields:
        //   - h.io.type_a : MxTypeBundle()
        //   - h.io.type_w : MxTypeBundle()
        //   - h.io.mode   : mxMode()
        //
        // Replace the bodies below with your real field pokes.
        // ------------------------------------------------------------------
        def pokeTypesAndMode(aType: Int, aAlt: Boolean, wType: Int, wAlt: Boolean): Unit = {
            // aType/wType are the mx_format encodings used in your RTL:
            //   0 -> FP4
            //   1 -> FP6
            //   2 -> FP8
            //
            // aAlt/wAlt are NOT used by the current RTL hookup (typeA/typeW only depend on mx_format),
            // but we keep them in the signature so the rest of the test stays the same.

            def expSigFromMxFormat(fmt: Int, alt: Boolean): (Int, Int) = fmt match {
                case 0 => (2, 2) // FP4
                case 1 => if (alt) (3, 3) else (2, 4) // FP6: E3M2 vs E2M3
                case 2 => if (alt) (5, 3) else (4, 4) // FP8: E5M2 vs E4M3
                case other => throw new IllegalArgumentException(s"bad mx_format=$other")
            }

            val (aExp, aSig) = expSigFromMxFormat(aType, aAlt)
            val (wExp, wSig) = expSigFromMxFormat(wType, wAlt)

            // Poke type_a/type_w (MxTypeBundle has fields exp/sig per your snippet)
            h.io.type_a.exp.poke(aExp.U)
            h.io.type_a.sig.poke(aSig.U)

            h.io.type_w.exp.poke(wExp.U)
            h.io.type_w.sig.poke(wSig.U)


        }

        for ((aName, aType, aAlt, aGen) <- aVariants) {
          for ((wName, wType, wAlt, wGen) <- wVariants) {
            println(s"\n==== Combo: $aName  vs  $wName  (a_type=$aType alt=${if(aAlt)1 else 0}; w_type=$wType alt=${if(wAlt)1 else 0}) ====")
              for (t <- 0 until trialsPerCombo) {
                // --------------------- Generate stimuli ---------------------
                val aRaws = aGen()
                val (aPacked, aVals, aDesc) = packActs(aType, aAlt, aRaws)

                val wRaws = wGen()
                val (wPacked, wVals, wDesc) = packWeis(wType, wAlt, wRaws)

                // Under your new throughput rules, mixed fp8 with fp4/fp6 is probably invalid.
                // We still run combos, but expectations only apply when lane rules match.
                val cRaw  = genBF16()
                val cVal  = bf16ToFloat(cRaw)

                // --------------------- PRE-TEST PRINTS ----------------------
                println(f"-- trial #$t%02d  PRE")
                println(s"  in_activation  (${aW}b) = ${binStr(aPacked, aW)}   $aDesc")
                println(s"  in_weights     (${wW}b) = ${binStr(wPacked, wW)}   $wDesc")
                showBF16("  c_raw (BF16)           ", cRaw)

                val expOpt = Array.tabulate(4)(i => expectedBF16Lane(aVals, wVals, cVal, i))
                (0 until 4).foreach { i =>
                  expOpt(i) match {
                    case Some(e) => showBF16(f"  exp lane[$i]", e)
                    case None    => println(f"  exp lane[$i]: (n/a)")
                  }
                }

                // --------------------- Drive DUT ----------------------------
                pokeTypesAndMode(aType, aAlt, wType, wAlt)
                h.io.in_activation.poke(aPacked.U(aW.W))
                h.io.in_weights.poke(wPacked.U(wW.W))
                h.io.c_raw.poke(cRaw.U(16.W))

                // latency cushion
                h.clock.step(2)

                // --------------------- POST-STEP PRINTS (before asserts) ----
                val outBits = h.io.out_bf16.peek().litValue
                println(s"  out_bf16 (${4*laneW}b)      = ${binStr(outBits, 4*laneW)}")
                val got = Array.tabulate(4)(i => lane(outBits, i, laneW))
                (0 until 4).foreach(i => showBF16(f"  got lane[$i]", got(i)))

                // --------------------- ASSERTIONS ---------------------------
                val filledExp = Array.tabulate(4) { i => expOpt(i).getOrElse(got(i)) }
                val packedExp =
                  (BigInt(filledExp(3) & 0xFFFF) << 48) |
                  (BigInt(filledExp(2) & 0xFFFF) << 32) |
                  (BigInt(filledExp(1) & 0xFFFF) << 16) |
                  BigInt(filledExp(0) & 0xFFFF)

                expOpt.zipWithIndex.foreach {
                  case (Some(e), i) =>
                    assert(got(i) == e, f"lane[$i] mismatch: got 0x${got(i)}%04X exp 0x$e%04X")
                  case _ => // don't-care
                }

                h.io.out_bf16.expect(packedExp.U, s"trial $t packed expect mismatch")
                println(s"  RESULT: PASS (trial $t)")
              }
          }
        }

        h.io.enable.poke(false.B)
        h.clock.step(1)
      }
  }

  // -----------------------------------------------------------------------
  // User-defined 12-bit activation / weight probe
  // Edit `userCases` below to inject any raw 12-bit packed values and read
  // the 4-lane BF16 output.
  //
  // Packing convention (FP6 E3M2, same as the randomised test above):
  //   in_activation / in_weights are split into two halves by pack2IntoHalves:
  //     bits [half-1 : 0]      -> element 0
  //     bits [totalW-1 : half] -> element 1
  //   where half = portWidth / 2.
  //
  //   For a 12-bit port: half = 6, element 0 in [5:0], element 1 in [11:6].
  //   For a 16-bit port: half = 8, element 0 in [5:0] (zero-padded to 8b),
  //                               element 1 in [13:8] (zero-padded to 8b).
  //
  // c_raw is a raw BF16 (E8M7) integer, e.g. 0x3F80 = 1.0, 0x0000 = 0.0.
  // -----------------------------------------------------------------------
  it should "run user-defined 12-bit activation and weight inputs and print lane outputs" in {

    val config = MxConfig.all.copy(
      productFormat = MxFormat(8, 8),
      accFormat = MxFormat(8, 8)
    )

    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

        // ---- helpers (same as above) ----
        def bf16ToFloat(raw16: Int): scala.Float = java.lang.Float.intBitsToFloat(raw16 << 16)
        def binStr(x: BigInt, w: Int): String = {
          val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
        }
        def laneVal(bits: BigInt, idx: Int, laneW: Int): Int =
          ((bits >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)).toInt
        def showBF16(tag: String, v: Int): Unit = {
          val s = (v >>> 15) & 1
          val e = (v >>> 7)  & 0xFF
          val f = v & 0x7F
          println(f"$tag: 0x$v%04X  s=$s e=0x$e%02X f=0x$f%02X  (~=${bf16ToFloat(v)}%g)")
        }

        val aW    = h.io.in_activation.getWidth
        val wW    = h.io.in_weights.getWidth
        val outLaneW = 16 // BF16 output lanes

        // ====================================================================
        // USER INPUTS — edit this table to test your own values.
        //
        // Each row: (label, activation_raw, weight_raw, c_raw_bf16)
        //   activation_raw : BigInt — raw bits for in_activation (12 b or aW b)
        //   weight_raw     : BigInt — raw bits for in_weights    (12 b or wW b)
        //   c_raw_bf16     : Int    — raw BF16 integer for the accumulate input
        // ====================================================================
        val userCases: Seq[(String, BigInt, BigInt, Int)] = Seq(
          // label            activation (12b)   weight (12b)    c_raw (BF16)
          ("user case 0",  BigInt("B6D", 16),  BigInt("02D", 16),  0x0000),  // c = 1.0
          //("user case 1",  BigInt("041", 16),  BigInt("041", 16),  0x0000),  // c = 0.0
          //("user case 2",  BigInt("FFF", 16),  BigInt("000", 16),  0x0000),  // max act, zero wei
        )


        h.io.type_a.exp.poke(3.U)
        h.io.type_a.sig.poke(3.U)
        h.io.type_w.exp.poke(3.U)
        h.io.type_w.sig.poke(3.U)
        h.io.enable.poke(true.B)

        for ((label, actRaw, weiRaw, cRaw) <- userCases) {
          println(s"\n==== $label ====")
          println(s"  in_activation (${aW}b) = ${binStr(actRaw, aW)}   hex=0x${actRaw.toString(16).toUpperCase}")
          println(s"  in_weights    (${wW}b) = ${binStr(weiRaw, wW)}   hex=0x${weiRaw.toString(16).toUpperCase}")
          showBF16("  c_raw (BF16)         ", cRaw)

          h.io.in_activation.poke(actRaw.U(aW.W))
          h.io.in_weights.poke(weiRaw.U(wW.W))
          h.io.c_raw.poke(cRaw.U(16.W))

          h.clock.step(2)

          // --- raw recFN(8,8) per lane: sign(1)|exp_recoded(9)|mant(7) = 17b ---
          val recFnBits  = h.io.out_recfn.peek().litValue
          val recFnLaneW = h.io.out_recfn.getWidth / 4   // 17
          println(s"  out_recfn (${h.io.out_recfn.getWidth}b) = ${binStr(recFnBits, h.io.out_recfn.getWidth)}")
          (0 until 4).foreach { i =>
            val rv   = laneVal(recFnBits, i, recFnLaneW)
            val sign = (rv >> (recFnLaneW - 1)) & 1
            val exp  = (rv >> 7) & 0x1FF   // bits [15:7] — 9-bit recoded exp
            val mant = rv & 0x7F           // bits [6:0]  — 7-bit mantissa
            println(f"  recFN lane[$i]: 0x${rv}%05X  s=$sign exp=0x$exp%03X(${exp}d) mant=0x$mant%02X")
          }

          // --- BF16 converted output ---
          val outBits = h.io.out_bf16.peek().litValue
          println(s"  out_bf16  (${4 * outLaneW}b) = ${binStr(outBits, 4 * outLaneW)}")
          val got = Array.tabulate(4)(i => laneVal(outBits, i, outLaneW))
          (0 until 4).foreach(i => showBF16(f"  BF16 lane[$i]", got(i)))
        }

        h.io.enable.poke(false.B)
        h.clock.step(1)
      }
  }
}

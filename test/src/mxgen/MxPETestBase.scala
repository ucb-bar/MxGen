package mxgen

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import hardfloat._

/** Shared test infrastructure for per-config MxFpMul tests.
  *
  * Concrete test specs extend this trait and override:
  *   - `testConfig`   : the MxConfig to elaborate
  *   - `testLabel`    : scalatest `behavior of` description
  *   - `actFormats`   : activation format variants to test
  *   - `weiFormats`   : weight format variants to test
  *   - `sameFormatOnly` (optional): restrict to same-format pairs
  *   - `trialsPerCombo` (optional): number of random trials per combo
  */
trait MxPETestBase extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ---------- subclass knobs ----------
  def testConfig: MxConfig
  def testLabel: String
  def actFormats: Seq[FormatDesc]
  def weiFormats: Seq[FormatDesc]
  def sameFormatOnly: Boolean = false
  def trialsPerCombo: Int = 10

  // ---------- small-format helpers ----------
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

  // ---------- format descriptors ----------
  case class FormatDesc(name: String, typeCode: Int, alt: Boolean, fmt: MiniFmt) {
    def sigWidth: Int = fmt.mBits + 1
    def isDual: Boolean = sigWidth < 4
  }
  val FmtFP4  = FormatDesc("fp4",        0, false, FP4_E2M1)
  val FmtE2M3 = FormatDesc("fp6 (E2M3)", 1, false, FP6_E2M3)
  val FmtE3M2 = FormatDesc("fp6 (E3M2)", 1, true,  FP6_E3M2)
  val FmtE4M3 = FormatDesc("fp8 (E4M3)", 2, false, FP8_E4M3)
  val FmtE5M2 = FormatDesc("fp8 (E5M2)", 2, true,  FP8_E5M2)

  // ---------- decode / encode ----------
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

  // ---------- printers ----------
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

  // ---------- random generators ----------
  def genSmall(rng: Random, fmt: MiniFmt): Int = {
    val r = rng.nextFloat()
    if (r < 0.10f) fmt.enc(0, 0)
    else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask))
    else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1),
                 rng.nextInt(fmt.mantMask + 1))
  }
  def genBF16(rng: Random): Int = {
    val r = rng.nextFloat()
    if (r < 0.10f) 0x0000
    else if (r < 0.20f) (0x0001 + rng.nextInt(0x7F))
    else {
      val mag = math.pow(2.0, rng.nextInt(8) - 4).toFloat
      val base = rng.nextFloat() * mag
      floatToBf16Raw(base)
    }
  }

  // ---------- packing ----------
  def pack2IntoHalves(raw0: Int, raw1: Int, elemBits: Int, totalW: Int): BigInt = {
    val half = totalW / 2
    require(totalW % 2 == 0, s"totalW=$totalW must be even")
    require(elemBits <= half, s"elemBits=$elemBits must fit in half=$half")
    val m = (1 << elemBits) - 1
    (BigInt(raw0 & m)) | (BigInt(raw1 & m) << half)
  }

  def packActs(aType: Int, aAlt: Boolean, raws: Seq[Int], aW: Int): (BigInt, Seq[Float], String) = {
    aType match {
      case 0 =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, aW)
        val vals = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
        (packed, vals, f"fp4 acts: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

      case 1 if aAlt =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, aW)
        val vals = raws.map(r => decodeSmall(FP6_E3M2, r & 0x3F))
        (packed, vals, s"fp6 E3M2 acts: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

      case 1 =>
        require(raws.length == 1)
        val a0 = raws.head & 0x3F
        val packed = BigInt(a0)
        val vals = Seq(decodeSmall(FP6_E2M3, a0))
        (packed, vals, f"fp6 E2M3 act: 0x$a0%02X -> ${vals.head}")

      case 2 if aAlt =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 8, aW)
        val vals = raws.map(r => decodeSmall(FP8_E5M2, r & 0xFF))
        (packed, vals, s"fp8 E5M2 acts: ${raws.map(r => f"0x${r & 0xFF}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

      case 2 =>
        require(raws.length == 1)
        val a0 = raws.head & 0xFF
        val packed = BigInt(a0)
        val vals = Seq(decodeSmall(FP8_E4M3, a0))
        (packed, vals, f"fp8 E4M3 act: 0x$a0%02X -> ${vals.head}")
    }
  }

  def packWeis(wType: Int, wAlt: Boolean, raws: Seq[Int], wW: Int): (BigInt, Seq[Float], String) = {
    wType match {
      case 0 =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, wW)
        val vals = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
        (packed, vals, f"fp4 weis: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

      case 1 if wAlt =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, wW)
        val vals = raws.map(r => decodeSmall(FP6_E3M2, r & 0x3F))
        (packed, vals, s"fp6 E3M2 weis: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

      case 1 =>
        require(raws.length == 1)
        val w0 = raws.head & 0x3F
        val packed = BigInt(w0)
        val vals = Seq(decodeSmall(FP6_E2M3, w0))
        (packed, vals, f"fp6 E2M3 wei: 0x$w0%02X -> ${vals.head}")

      case 2 if wAlt =>
        require(raws.length == 2)
        val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 8, wW)
        val vals = raws.map(r => decodeSmall(FP8_E5M2, r & 0xFF))
        (packed, vals, s"fp8 E5M2 weis: ${raws.map(r => f"0x${r & 0xFF}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

      case 2 =>
        require(raws.length == 1)
        val w0 = raws.head & 0xFF
        val packed = BigInt(w0)
        val vals = Seq(decodeSmall(FP8_E4M3, w0))
        (packed, vals, f"fp8 E4M3 wei: 0x$w0%02X -> ${vals.head}")
    }
  }

  // ---------- expected output ----------
  def expectedBF16Lane(aVals: Seq[Float], wVals: Seq[Float], c: Float, lane: Int): Option[Int] = {
    def toBF16(x: Float) = floatToBf16Raw(x)
    (aVals.length, wVals.length) match {
      case (2, 2) =>
        val pairs = Array((0,0), (0,1), (1,0), (1,1))
        val (ai, wi) = pairs(lane)
        Some(toBF16(aVals(ai) * wVals(wi) + c))
      case (2, 1) =>
        lane match {
          case 0 | 1 => Some(toBF16(aVals(0) * wVals(0) + c))
          case 2 | 3 => Some(toBF16(aVals(1) * wVals(0) + c))
          case _     => None
        }
      case (1, 2) =>
        lane match {
          case 0 | 1 => Some(toBF16(aVals(0) * wVals(0) + c))
          case 2 | 3 => Some(toBF16(aVals(0) * wVals(1) + c))
          case _     => None
        }
      case (1, 1) =>
        if (lane == 0) Some(toBF16(aVals(0) * wVals(0) + c)) else None
      case _ => None
    }
  }

  // ---------- DUT driving ----------
  def pokeTypesAndMode(h: MxFpMulHarnessBf16Out_NewIO, aType: Int, aAlt: Boolean, wType: Int, wAlt: Boolean): Unit = {
    def expSigFromMxFormat(fmt: Int, alt: Boolean): (Int, Int) = fmt match {
      case 0 => (2, 2)
      case 1 => if (alt) (3, 3) else (2, 4)
      case 2 => if (alt) (5, 3) else (4, 4)
      case other => throw new IllegalArgumentException(s"bad mx_format=$other")
    }
    val (aExp, aSig) = expSigFromMxFormat(aType, aAlt)
    val (wExp, wSig) = expSigFromMxFormat(wType, wAlt)
    h.io.type_a.exp.poke(aExp.U)
    h.io.type_a.sig.poke(aSig.U)
    h.io.type_w.exp.poke(wExp.U)
    h.io.type_w.sig.poke(wSig.U)
  }

  // ---------- variant builder ----------
  def buildVariants(prefix: String, formats: Seq[FormatDesc], rng: Random): Seq[(String, Int, Boolean, () => Seq[Int])] = {
    formats.map { fd =>
      val count = if (fd.isDual) 2 else 1
      val label = s"$prefix: ${count}\u00d7${fd.name}"
      val gen: () => Seq[Int] = () => Seq.fill(count)(genSmall(rng, fd.fmt))
      (label, fd.typeCode, fd.alt, gen)
    }
  }

  // ---------- the test ----------
  behavior of testLabel

  it should "run randomized combos and self-check lanes" in {
    val config = testConfig

    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

        val rng = new Random(0xBEEFBABE)
        val aW = h.io.in_activation.getWidth
        val wW = h.io.in_weights.getWidth
        val laneW = 16

        val aVariants = buildVariants("A", actFormats, rng)
        val wVariants = buildVariants("W", weiFormats, rng)

        h.io.enable.poke(true.B)

        for ((aName, aType, aAlt, aGen) <- aVariants;
             (wName, wType, wAlt, wGen) <- wVariants;
             if !sameFormatOnly || (aType == wType && aAlt == wAlt)) {

          println(s"\n==== Combo: $aName  vs  $wName  (a_type=$aType alt=${if(aAlt)1 else 0}; w_type=$wType alt=${if(wAlt)1 else 0}) ====")

          for (t <- 0 until trialsPerCombo) {
            val aRaws = aGen()
            val (aPacked, aVals, aDesc) = packActs(aType, aAlt, aRaws, aW)

            val wRaws = wGen()
            val (wPacked, wVals, wDesc) = packWeis(wType, wAlt, wRaws, wW)

            val cRaw = genBF16(rng)
            val cVal = bf16ToFloat(cRaw)

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

            pokeTypesAndMode(h, aType, aAlt, wType, wAlt)
            h.io.in_activation.poke(aPacked.U(aW.W))
            h.io.in_weights.poke(wPacked.U(wW.W))
            h.io.c_raw.poke(cRaw.U(16.W))

            h.clock.step(2)

            val outBits = h.io.out_bf16.peek().litValue
            println(s"  out_bf16 (${4*laneW}b)      = ${binStr(outBits, 4*laneW)}")
            val got = Array.tabulate(4)(i => laneVal(outBits, i, laneW))
            (0 until 4).foreach(i => showBF16(f"  got lane[$i]", got(i)))

            val filledExp = Array.tabulate(4) { i => expOpt(i).getOrElse(got(i)) }
            val packedExp =
              (BigInt(filledExp(3) & 0xFFFF) << 48) |
              (BigInt(filledExp(2) & 0xFFFF) << 32) |
              (BigInt(filledExp(1) & 0xFFFF) << 16) |
              BigInt(filledExp(0) & 0xFFFF)

            expOpt.zipWithIndex.foreach {
              case (Some(e), i) =>
                assert(got(i) == e, f"lane[$i] mismatch: got 0x${got(i)}%04X exp 0x$e%04X")
              case _ =>
            }

            h.io.out_bf16.expect(packedExp.U, s"trial $t packed expect mismatch")
            println(s"  RESULT: PASS (trial $t)")
          }
        }

        h.io.enable.poke(false.B)
        h.clock.step(1)
      }
  }
}

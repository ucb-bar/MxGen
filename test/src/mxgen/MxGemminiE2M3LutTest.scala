package mxgen

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

/** mode9 for E2M3 (fp6, exp2 sig4): 2 E2M3 acts x 2 E2M3 weights -> 4 independent products, via the same
  * 16-MACU quad PE as E4M3 (both sig4). E2M3 is 4-wide-via-LUT only. productFormat/accFormat = BF16 (8,8)
  * is lossless for E2M3xE2M3, so the golden is the exact float product + c rounded to BF16.
  */
class MxFpMul_MxGemminiE4M3Lut_Mode9_E2M3_Spec
    extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ---- E2M3 (exp2 man3 bias1), 6-bit: sign(5), exp(4:3), man(2:0) ----
  val mBits = 3; val bias = 1
  val expMask = 0x3; val mantMask = 0x7
  def decodeE2M3(raw: Int): Float = {
    val e = (raw >> mBits) & expMask
    val m = raw & mantMask
    if (e == 0) (m.toFloat / (1 << mBits)) * math.pow(2.0, 1 - bias).toFloat
    else (1.0f + m.toFloat / (1 << mBits)) * math.pow(2.0, e - bias).toFloat
  }
  def genE2M3(rng: Random): Int = {
    val r = rng.nextFloat()
    if (r < 0.12f) 0
    else if (r < 0.28f) 1 + rng.nextInt(mantMask)                 // subnormal
    else ((1 + rng.nextInt(expMask)) << mBits) | rng.nextInt(mantMask + 1)  // normal, exp 1..3
  }

  def bf16ToFloat(raw16: Int): Float = java.lang.Float.intBitsToFloat(raw16 << 16)
  def floatToBf16Raw(f: Float): Int = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lsb  = (bits >>> 16) & 1
    (((bits + (0x7FFF + lsb)) >>> 16) & 0xFFFF)
  }
  def genBF16(rng: Random): Int =
    if (rng.nextFloat() < 0.2f) 0
    else floatToBf16Raw(rng.nextFloat() * math.pow(2.0, rng.nextInt(4) - 2).toFloat)

  behavior of "MxFpMul mxGemminiE4M3Lut mode9 (E2M3 4-wide)"

  it should "compute 4 independent E2M3 products with lut_en" in {
    val config = MxConfig.mxGemminiE4M3Lut.copy(
      productFormat = MxFormat(8, 8), accFormat = MxFormat(8, 8))

    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false, latency = 0, forceLutEn = true)) { h =>
      h.io.enable.poke(true.B)
      h.io.type_a.exp.poke(2.U); h.io.type_a.sig.poke(4.U)   // E2M3
      h.io.type_w.exp.poke(2.U); h.io.type_w.sig.poke(4.U)

      val rng = new Random(0xE2A3B1CD)
      val pairs = Array((0, 0), (0, 1), (1, 0), (1, 1))

      for (t <- 0 until 300) {
        val a = Seq.fill(2)(genE2M3(rng))
        val w = Seq.fill(2)(genE2M3(rng))
        val cRaw = genBF16(rng); val cVal = bf16ToFloat(cRaw)

        // 2 E2M3 (6-bit) in the low 6 of each 8-bit half of the 16-bit lane.
        val aPacked = BigInt(a(0) & 0x3F) | (BigInt(a(1) & 0x3F) << 8)
        val wPacked = BigInt(w(0) & 0x3F) | (BigInt(w(1) & 0x3F) << 8)

        h.io.in_activation.poke(aPacked.U(16.W))
        h.io.in_weights.poke(wPacked.U(16.W))
        h.io.c_raw.poke(cRaw.U(16.W))
        h.clock.step(2)

        val out = h.io.out_bf16.peek().litValue
        for (k <- 0 until 4) {
          val (ai, wj) = pairs(k)
          val exp = floatToBf16Raw(decodeE2M3(a(ai)) * decodeE2M3(w(wj)) + cVal)
          val got = ((out >> (k * 16)) & 0xFFFF).toInt
          assert(got == exp,
            f"trial $t lane $k: got 0x$got%04X exp 0x$exp%04X  " +
            f"(a$ai=0x${a(ai)}%02X=${decodeE2M3(a(ai))} w$wj=0x${w(wj)}%02X=${decodeE2M3(w(wj))} c=$cVal)")
        }
      }
    }
  }
}

package mxgen

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

/** mode9 (E4M3 4-wide via the 16-MACU quad PE): 2 E4M3 acts x 2 E4M3 weights ->
  * 4 independent products [a0w0, a0w1, a1w0, a1w1]. lut_en forces mode9.
  * productFormat/accFormat = BF16 (8,8) is lossless for E4M3xE4M3, so the golden
  * is the exact float product + c rounded to BF16.
  */
class MxFpMul_MxGemminiE4M3Lut_Mode9_Spec
    extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ---- E4M3 (e4 m3 bias7) ----
  val mBits = 3; val bias = 7
  val expMask = 0xF; val mantMask = 0x7
  def decodeE4M3(raw: Int): Float = {
    val e = (raw >> mBits) & expMask
    val m = raw & mantMask
    if (e == 0) {
      if (m == 0) 0.0f else (m.toFloat / (1 << mBits)) * math.pow(2.0, 1 - bias).toFloat
    } else (1.0f + m.toFloat / (1 << mBits)) * math.pow(2.0, e - bias).toFloat
  }
  def genE4M3(rng: Random): Int = {
    val r = rng.nextFloat()
    if (r < 0.12f) 0                                              // zero
    else if (r < 0.28f) 1 + rng.nextInt(mantMask)                // subnormal
    else ((1 + rng.nextInt(expMask - 1)) << mBits) | rng.nextInt(mantMask + 1)  // normal, exp 1..14
  }

  def bf16ToFloat(raw16: Int): Float = java.lang.Float.intBitsToFloat(raw16 << 16)
  def floatToBf16Raw(f: Float): Int = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lsb  = (bits >>> 16) & 1
    (((bits + (0x7FFF + lsb)) >>> 16) & 0xFFFF)
  }
  def genBF16(rng: Random): Int = {
    if (rng.nextFloat() < 0.2f) 0
    else floatToBf16Raw(rng.nextFloat() * math.pow(2.0, rng.nextInt(6) - 3).toFloat)
  }

  behavior of "MxFpMul mxGemminiE4M3Lut mode9 (E4M3 4-wide)"

  it should "compute 4 independent E4M3 products with lut_en" in {
    val config = MxConfig.mxGemminiE4M3Lut.copy(
      productFormat = MxFormat(8, 8), accFormat = MxFormat(8, 8))

    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false, latency = 0, forceLutEn = true)) { h =>
      h.io.enable.poke(true.B)
      h.io.type_a.exp.poke(4.U); h.io.type_a.sig.poke(4.U)
      h.io.type_w.exp.poke(4.U); h.io.type_w.sig.poke(4.U)

      val rng = new Random(0x4E3BABE)
      val pairs = Array((0, 0), (0, 1), (1, 0), (1, 1))

      for (t <- 0 until 300) {
        val a = Seq.fill(2)(genE4M3(rng))
        val w = Seq.fill(2)(genE4M3(rng))
        val cRaw = genBF16(rng); val cVal = bf16ToFloat(cRaw)

        val aPacked = BigInt(a(0) & 0xFF) | (BigInt(a(1) & 0xFF) << 8)
        val wPacked = BigInt(w(0) & 0xFF) | (BigInt(w(1) & 0xFF) << 8)

        h.io.in_activation.poke(aPacked.U(16.W))
        h.io.in_weights.poke(wPacked.U(16.W))
        h.io.c_raw.poke(cRaw.U(16.W))
        h.clock.step(2)

        val out = h.io.out_bf16.peek().litValue
        for (k <- 0 until 4) {
          val (ai, wj) = pairs(k)
          val exp = floatToBf16Raw(decodeE4M3(a(ai)) * decodeE4M3(w(wj)) + cVal)
          val got = ((out >> (k * 16)) & 0xFFFF).toInt
          assert(got == exp,
            f"trial $t lane $k: got 0x$got%04X exp 0x$exp%04X  " +
            f"(a${ai}=0x${a(ai)}%02X=${decodeE4M3(a(ai))} w${wj}=0x${w(wj)}%02X=${decodeE4M3(w(wj))} c=$cVal)")
        }
      }
    }
  }
}

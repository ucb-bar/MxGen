package mxgen

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

/** mode11 (mixed quad, mirror of mode10): 2 FP4 acts x 2 E4M3 weights -> 4
  * independent products via the LaneMul asymmetric 4xN path (E4M3 is the wide
  * operand). lut_en forces mode11. productFormat/accFormat = BF16 (8,8).
  */
class MxFpMul_Fp4E4M3_Mode11_Spec
    extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  def decodeE4M3(raw: Int): Float = {
    val e = (raw >> 3) & 0xF; val m = raw & 0x7
    if (e == 0) { if (m == 0) 0.0f else (m.toFloat / 8) * math.pow(2.0, 1 - 7).toFloat }
    else (1.0f + m.toFloat / 8) * math.pow(2.0, e - 7).toFloat
  }
  def genE4M3(rng: Random): Int = {
    val r = rng.nextFloat()
    if (r < 0.12f) 0
    else if (r < 0.28f) 1 + rng.nextInt(0x7)
    else ((1 + rng.nextInt(0xE)) << 3) | rng.nextInt(0x8)
  }
  def decodeFP4(raw: Int): Float = {
    val e = (raw >> 1) & 0x3; val m = raw & 0x1
    if (e == 0) { if (m == 0) 0.0f else (m.toFloat / 2) * math.pow(2.0, 1 - 1).toFloat }
    else (1.0f + m.toFloat / 2) * math.pow(2.0, e - 1).toFloat
  }
  def genFP4(rng: Random): Int = rng.nextInt(8)

  def bf16ToFloat(raw16: Int): Float = java.lang.Float.intBitsToFloat(raw16 << 16)
  def floatToBf16Raw(f: Float): Int = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lsb  = (bits >>> 16) & 1
    (((bits + (0x7FFF + lsb)) >>> 16) & 0xFFFF)
  }
  def genBF16(rng: Random): Int =
    if (rng.nextFloat() < 0.2f) 0
    else floatToBf16Raw(rng.nextFloat() * math.pow(2.0, rng.nextInt(6) - 3).toFloat)

  behavior of "MxFpMul FP4 x E4M3 mode11 (mixed quad, 4 products)"

  it should "compute 4 independent FP4 x E4M3 products with lut_en" in {
    val config = MxConfig(Set(MxFormat.FP4), Set(MxFormat.FP8_E4M3),
      productFormat = MxFormat(8, 8), accFormat = MxFormat(8, 8),
      inActBusWidth = 8, inWeiBusWidth = 16,
      modesOverride = Some(List(MxPEParams.mode11)),
      expAdderWidths = Seq(4, 4, 4, 4))

    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false, latency = 0, forceLutEn = true)) { h =>
      h.io.enable.poke(true.B)
      h.io.type_a.exp.poke(2.U); h.io.type_a.sig.poke(2.U)   // FP4
      h.io.type_w.exp.poke(4.U); h.io.type_w.sig.poke(4.U)   // E4M3

      val rng = new Random(0xF43E4)
      val pairs = Array((0, 0), (0, 1), (1, 0), (1, 1))

      for (t <- 0 until 300) {
        val a = Seq.fill(2)(genFP4(rng))    // 4-bit FP4 codes
        val w = Seq.fill(2)(genE4M3(rng))   // 8-bit E4M3 codes
        val cRaw = genBF16(rng); val cVal = bf16ToFloat(cRaw)

        val aPacked = BigInt(a(0) & 0xF)  | (BigInt(a(1) & 0xF)  << 4)   // 2 x 4b
        val wPacked = BigInt(w(0) & 0xFF) | (BigInt(w(1) & 0xFF) << 8)   // 2 x 8b

        h.io.in_activation.poke(aPacked.U(8.W))
        h.io.in_weights.poke(wPacked.U(16.W))
        h.io.c_raw.poke(cRaw.U(16.W))
        h.clock.step(2)

        val out = h.io.out_bf16.peek().litValue
        for (k <- 0 until 4) {
          val (ai, wj) = pairs(k)
          val exp = floatToBf16Raw(decodeFP4(a(ai)) * decodeE4M3(w(wj)) + cVal)
          val got = ((out >> (k * 16)) & 0xFFFF).toInt
          assert(got == exp,
            f"trial $t lane $k: got 0x$got%04X exp 0x$exp%04X  " +
            f"(a${ai}=0x${a(ai)}%01X=${decodeFP4(a(ai))} w${wj}=0x${w(wj)}%02X=${decodeE4M3(w(wj))} c=$cVal)")
        }
      }
    }
  }
}

package mxgen
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Sweep products of known 2^-e magnitude through the PE at accFormat=(8,8) to find where the
  * subnormal-product exponent wraps/saturates (correct tiny value vs 448-style blowup). */
class MxFp8WrapSweep extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  val cfg = MxConfig.mxGemmini.copy(productFormat = MxFormat(4,4), accFormat = MxFormat(8,8))
  // (Acode, Bcode, label = product 2^-e)
  val pairs = Seq(
    (0x38,0x38,"2^0"), (0x38,0x08,"2^-6"), (0x08,0x08,"2^-12"),
    (0x08,0x04,"2^-13"), (0x08,0x02,"2^-14"), (0x08,0x01,"2^-15"),
    (0x04,0x01,"2^-16"), (0x02,0x01,"2^-17"), (0x01,0x01,"2^-18"),
    (0x30,0x01,"2^-10"), (0x28,0x01,"2^-11"), (0x20,0x01,"2^-11b"),
    (0x38,0x01,"2^-9"), (0x38,0x02,"2^-8"), (0x38,0x04,"2^-7"),
    (0x04,0x02,"2^-15b"), (0x04,0x04,"2^-14b"), (0x02,0x02,"2^-16b")
  )
  behavior of "wrap sweep at (8,8)"
  it should "print product out per magnitude" in {
    test(new MxFpMulHarnessBf16Out_NewIO(cfg, lut=false, latency=0)) { h =>
      h.io.type_a.exp.poke(4.U); h.io.type_a.sig.poke(4.U)
      h.io.type_w.exp.poke(4.U); h.io.type_w.sig.poke(4.U)
      h.io.enable.poke(true.B)
      val aW=h.io.in_activation.getWidth; val wW=h.io.in_weights.getWidth
      for ((a,b,lab) <- pairs) {
        h.io.in_activation.poke(BigInt(a&0xFF).U(aW.W))
        h.io.in_weights.poke(BigInt(b&0xFF).U(wW.W))
        h.io.c_raw.poke(0.U(16.W))
        h.clock.step(1)
        val out=h.io.out_bf16.peek().litValue.toInt & 0xFFFF
        println(f"[SWEEP] A=0x$a%02X B=0x$b%02X ($lab%-7s) out=0x$out%04X")
      }
      h.io.enable.poke(false.B)
    }
  }
}

package mxgen

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Isolated replay of the fp8 mesh-column accumulate at accFormat = E4M5 = (4,5), i.e. rows 0-7
  * of meshAccPrecisionList (the format where the fp8_64x64 test's Inf appears). It chains a
  * sequence of (fp8 E4M3 A, B) operands with c = previous output -- exactly like one mesh column
  * -- and PRINTS the running BF16 accumulator each step, flagging Inf.
  *
  * Purpose: reproduce, in isolation (no full VCS build), the accumulate that overflows to Inf on
  * the RTL where Spike stays finite. Seed `vectors` from the Spike trace: build+run the fp8 test
  * on Spike (the `[SPK-ACC] ... Acode=.. Bcode=.. Ct=..` lines added to gemmini.cc for element
  * (1,11)) and paste the (Acode, Bcode) pairs below in kk order. Then compare each `[DUT]` line's
  * running `out` against Spike's `Ct` to find the first diverging step, iterate on the mxgen fix,
  * and re-run:
  *
  *   cd generators/gemmini/mxgen
  *   ./mill test.testOnly mxgen.MxFp8AccChainReplay
  *
  * NOTE: rows 8-15 use wider acc formats (E4M6/E4M7/E8M8). If the divergence is past kk=7, split
  * the chain and switch `config.accFormat` per segment (or add more harness instances).
  */
class MxFp8AccChainReplay extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // mxGemmini fp8 PE: productFormat = E4M3 = (4,4) sig incl. implicit; accFormat = E4M5 = (4,5).
  val config = MxConfig.mxGemmini.copy(productFormat = MxFormat(4, 4), accFormat = MxFormat(4, 5))

  // fp8 E4M3 type fields (sigWidth = mantissa + implicit bit).
  val E4M3_EXP = 4
  val E4M3_SIG = 4

  // (fp8 E4M3 A code, fp8 E4M3 B code) per K-step, in kk order.
  // From Spike [SPK-ACC], element (1,11), K-tile ko=0. Spike's running Ct (E4M4/am=4 for kk0-7):
  //   kk0=-0.203125 kk1=-0.2109375 kk2=-0.2421875 kk3=-0.0078125 kk4=0.0048828 kk5=0.0205078
  //   kk6=0.0029297 kk7=0.0029297 kk8=-0.0361328 kk9=-0.0244141 kk10=-0.0222168 kk11=0.166015625
  //   kk12=0.1640625 kk13=0.0234375 kk14=0.0263672 kk15=-0.0830078   (all finite, |Ct|<0.25)
  // acc schedule for this element: am=4 (kk0-7), am=5 (kk8-9), am=6 (kk10-14), ae=8/am=7 (kk15).
  // This harness is fixed at accFormat=(4,5)=E4M4(am=4) -- exact for kk0-7, narrower for kk8+.
  // Full ko=0 chain for element (0,0). Spike Ct per kk (am=4 for kk0-7 -> exact in this harness):
  //   kk0=-0.1171875 kk1=-0.1171875 kk2=-0.1171875 kk3=-0.1171875 kk4=-0.109375 kk5=-0.10546875
  //   kk6=-0.109375 kk7=-0.109375  (kk1..3 have ~2^-9 products that must be KEPT, not flushed)
  val vectors: Seq[(Int, Int)] = Seq(
    (0xa9, 0x2e), (0x8e, 0x99), (0x08, 0x99), (0x1e, 0x87),
    (0x07, 0x2d), (0x85, 0xad), (0x84, 0x2b), (0x80, 0xa9),
    (0xbe, 0x13), (0x8a, 0x2d), (0x1e, 0x2c), (0x87, 0x19),
    (0x25, 0x32), (0x84, 0x15), (0x25, 0x3a), (0x21, 0x34)
  )

  behavior of "fp8 acc-chain replay (accFormat=(4,5))"

  it should "print the running accumulator per K-step" in {
    test(new MxFpMulHarnessBf16Out_NewIO(config, lut = false, latency = 0)) { h =>
      h.io.type_a.exp.poke(E4M3_EXP.U); h.io.type_a.sig.poke(E4M3_SIG.U)
      h.io.type_w.exp.poke(E4M3_EXP.U); h.io.type_w.sig.poke(E4M3_SIG.U)
      h.io.enable.poke(true.B)

      val aW = h.io.in_activation.getWidth
      val wW = h.io.in_weights.getWidth
      var c  = 0 // BF16 raw accumulator, start at +0.0

      for (((aCode, bCode), kk) <- vectors.zipWithIndex) {
        h.io.in_activation.poke(BigInt(aCode & 0xFF).U(aW.W))
        h.io.in_weights.poke(BigInt(bCode & 0xFF).U(wW.W))
        h.io.c_raw.poke((c & 0xFFFF).U(16.W))
        h.clock.step(1)
        val out   = h.io.out_bf16.peek().litValue.toInt & 0xFFFF // lane 0 = the fp8 (numOutputs=1) result
        val isInf = ((out >> 7) & 0xFF) == 0xFF && (out & 0x7F) == 0
        println(f"[DUT] kk=$kk%2d Acode=0x$aCode%02X Bcode=0x$bCode%02X c=0x$c%04X -> out=0x$out%04X" +
          (if (isInf) "  <== INF" else ""))
        c = out
      }

      h.io.enable.poke(false.B)
    }
  }
}

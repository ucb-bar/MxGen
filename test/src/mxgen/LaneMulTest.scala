package mxgen

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exhaustive check of the LaneMul significand-multiply primitive:
  *  - is4x4  : 4-bit x 4-bit unsigned product (E4M3 significands)
  *  - 3x3    : FP6 significands (act/wei M2 modes on)
  *  - 2x2    : FP4 significands (modes off)
  */
class LaneMulSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "LaneMul"

  it should "compute exact 4x4 / 3x3 / 2x2 products" in {
    test(new LaneMul(lut = false)) { d =>
      d.io.enable.poke(true.B)

      // ---- 4x4 (E4M3): all 16x16 ----
      d.io.is4x4.poke(true.B)
      d.io.actMode.poke(false.B); d.io.weiMode.poke(false.B)
      for (a <- 0 until 16; w <- 0 until 16) {
        d.io.act.poke(a.U); d.io.wei.poke(w.U)
        d.clock.step()
        d.io.out.expect((a * w).U, s"4x4 $a*$w")
      }

      // ---- 3x3 (FP6): all 8x8 ----
      d.io.is4x4.poke(false.B)
      d.io.actMode.poke(true.B); d.io.weiMode.poke(true.B)
      for (a <- 0 until 8; w <- 0 until 8) {
        d.io.act.poke(a.U); d.io.wei.poke(w.U)
        d.clock.step()
        d.io.out.expect((a * w).U, s"3x3 $a*$w")
      }

      // ---- 2x2 (FP4): all 4x4 ----
      d.io.is4x4.poke(false.B)
      d.io.actMode.poke(false.B); d.io.weiMode.poke(false.B)
      for (a <- 0 until 4; w <- 0 until 4) {
        d.io.act.poke(a.U); d.io.wei.poke(w.U)
        d.clock.step()
        d.io.out.expect((a * w).U, s"2x2 $a*$w")
      }
    }
  }
}

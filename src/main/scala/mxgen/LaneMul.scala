package mxgen

import chisel3._
import chisel3.util._

/** One output-lane significand multiplier built from 4 MACUs.
  *
  *  - is4x4 = true  : full 4x4 (E4M3 significand x E4M3 significand). All 4 MACUs
  *                    act as plain 2x2s on the 2-bit halves; recombined with the
  *                    mode8 shift pattern  a0*w0 + (a0*w1 + a1*w0)<<2 + a1*w1<<4.
  *  - is4x4 = false : a single small product act x wei on MACU[0] only, using the
  *                    MACU M2 shift-adds for a 3-bit operand (FP6). FP4 (2-bit)
  *                    leaves both modes off. The other 3 MACUs idle.
  *
  * `act`/`wei` are right-aligned significands (leading + mantissa): 2b FP4, 3b FP6,
  * 4b E4M3. Output is the LSB-aligned unsigned product (<= 8 bits).
  */
class LaneMul(lut: Boolean) extends Module {
  val io = IO(new Bundle {
    val act     = Input(UInt(4.W))
    val wei     = Input(UInt(4.W))
    val is4x4   = Input(Bool())   // E4M3: 4-bit x 4-bit
    val actMode = Input(Bool())   // small path: act is 3-bit (FP6) -> MACU act M2
    val weiMode = Input(Bool())   // small path: wei is 3-bit (FP6) -> MACU w   M2
    val enable  = Input(Bool())
    val out     = Output(UInt(8.W))
  })

  val mac = Seq.fill(2, 2)(Module(new MACU(lut)))

  val a0 = io.act(1, 0); val a1 = io.act(3, 2)
  val w0 = io.wei(1, 0); val w1 = io.wei(3, 2)

  // MACU[0][0] is shared: in 4x4 it is the a0*w0 partial (plain 2x2); in the
  // single path it is the full act*wei 3x3 (M2 modes gated by act/weiMode).
  mac(0)(0).io.w        := Mux(io.is4x4, a0, io.act(2, 0))
  mac(0)(0).io.act      := Mux(io.is4x4, w0, io.wei(2, 0))
  mac(0)(0).io.w_mode   := Mux(io.is4x4, 0.U, io.actMode.asUInt)
  mac(0)(0).io.act_mode := Mux(io.is4x4, 0.U, io.weiMode.asUInt)
  mac(0)(0).io.enable   := io.enable

  // The other three MACUs only participate in the 4x4.
  mac(0)(1).io.w := a0; mac(0)(1).io.act := w1
  mac(1)(0).io.w := a1; mac(1)(0).io.act := w0
  mac(1)(1).io.w := a1; mac(1)(1).io.act := w1
  for (p <- Seq(mac(0)(1), mac(1)(0), mac(1)(1))) {
    p.io.w_mode := 0.U; p.io.act_mode := 0.U
    p.io.enable := io.enable && io.is4x4
  }

  val p00 = mac(0)(0).io.output   // a0*w0 (4x4)  OR  act*wei (single)
  val p01 = mac(0)(1).io.output   // a0*w1
  val p10 = mac(1)(0).io.output   // a1*w0
  val p11 = mac(1)(1).io.output   // a1*w1

  val out4x4   = (p00 +& ((p01 +& p10) << 2) +& (p11 << 4))(7, 0)
  val outSmall = p00.pad(8)

  io.out := Mux(io.is4x4, out4x4, outSmall)
}

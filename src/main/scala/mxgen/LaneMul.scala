package mxgen

import chisel3._
import chisel3.util._

/** One output-lane significand multiplier built from 4 MACUs.
  *
  *  - is4x4 = true  : full 4x4 (E4M3 significand x E4M3 significand). All 4 MACUs
  *                    act as plain 2x2s on the 2-bit halves; recombined with the
  *                    mode8 shift pattern  a0*w0 + (a0*w1 + a1*w0)<<2 + a1*w1<<4.
  *  - is4xN = true  : asymmetric quad -- one operand is 4-bit (E4M3), the other is
  *                    2-bit (FP4) or 3-bit (FP6). Split the 4-bit operand into
  *                    {hi,lo} 2-bit halves and use 2 MACUs: lo*s + (hi*s)<<2, where
  *                    the small operand s rides a MACU M2 shift-add when 3-bit.
  *  - both false    : a single small product act x wei on MACU[0] only, using the
  *                    MACU M2 shift-adds for a 3-bit operand (FP6). FP4 (2-bit)
  *                    leaves both modes off. The other 3 MACUs idle.
  *
  * `act`/`wei` are right-aligned significands (leading + mantissa): 2b FP4, 3b FP6,
  * 4b E4M3. Output is the LSB-aligned unsigned product (<= 8 bits).
  */
class LaneMul(lut: Boolean) extends Module {
  val io = IO(new Bundle {
    val act       = Input(UInt(4.W))
    val wei       = Input(UInt(4.W))
    val is4x4     = Input(Bool())   // E4M3: 4-bit x 4-bit
    val is4xN     = Input(Bool())   // asymmetric: 4-bit x (2- or 3-bit)
    val wideIsAct = Input(Bool())   // is4xN: the 4-bit operand is act (else wei)
    val actMode   = Input(Bool())   // small path / is4xN: act is 3-bit (FP6) -> M2
    val weiMode   = Input(Bool())   // small path / is4xN: wei is 3-bit (FP6) -> M2
    val enable    = Input(Bool())
    val out       = Output(UInt(8.W))
  })

  val mac = Seq.fill(2, 2)(Module(new MACU(lut)))

  val a0 = io.act(1, 0); val a1 = io.act(3, 2)
  val w0 = io.wei(1, 0); val w1 = io.wei(3, 2)

  // is4xN operands: split the 4-bit (wide) operand into 2-bit halves; the small operand (<=3 bits) goes
  // whole into the MACU act slot with act_mode = its 3-bit flag, so the MACU M2 path covers its 3rd bit.
  val wide     = Mux(io.wideIsAct, io.act, io.wei)
  val small    = Mux(io.wideIsAct, io.wei, io.act)(2, 0)
  val smallIs3 = Mux(io.wideIsAct, io.weiMode, io.actMode)
  val wLo = wide(1, 0); val wHi = wide(3, 2)

  // MACU[0][0]: 4x4 -> a0*w0 ; is4xN -> wLo*small ; single -> full act*wei 3x3.
  mac(0)(0).io.w        := Mux(io.is4x4, a0, Mux(io.is4xN, wLo,   io.act(2, 0)))
  mac(0)(0).io.act      := Mux(io.is4x4, w0, Mux(io.is4xN, small, io.wei(2, 0)))
  mac(0)(0).io.w_mode   := Mux(io.is4x4, 0.U, Mux(io.is4xN, 0.U,      io.actMode.asUInt))
  mac(0)(0).io.act_mode := Mux(io.is4x4, 0.U, Mux(io.is4xN, smallIs3.asUInt, io.weiMode.asUInt))
  mac(0)(0).io.enable   := io.enable

  // MACU[0][1]: 4x4 -> a0*w1 ; is4xN -> wHi*small ; else idle.
  mac(0)(1).io.w        := Mux(io.is4xN, wHi, a0)
  mac(0)(1).io.act      := Mux(io.is4xN, small, w1)
  mac(0)(1).io.w_mode   := 0.U
  mac(0)(1).io.act_mode := Mux(io.is4xN, smallIs3.asUInt, 0.U)
  mac(0)(1).io.enable   := io.enable && (io.is4x4 || io.is4xN)

  // MACU[1][0], MACU[1][1]: only the 4x4 uses these.
  mac(1)(0).io.w := a1; mac(1)(0).io.act := w0
  mac(1)(1).io.w := a1; mac(1)(1).io.act := w1
  for (p <- Seq(mac(1)(0), mac(1)(1))) {
    p.io.w_mode := 0.U; p.io.act_mode := 0.U
    p.io.enable := io.enable && io.is4x4
  }

  val p00 = mac(0)(0).io.output   // a0*w0 (4x4) | wLo*small (4xN) | act*wei (single)
  val p01 = mac(0)(1).io.output   // a0*w1 (4x4) | wHi*small (4xN)
  val p10 = mac(1)(0).io.output   // a1*w0
  val p11 = mac(1)(1).io.output   // a1*w1

  val out4x4   = (p00 +& ((p01 +& p10) << 2) +& (p11 << 4))(7, 0)
  val outAsym  = (p00 +& (p01 << 2))(7, 0)   // wLo*small + (wHi*small)<<2 = wide*small
  val outSmall = p00.pad(8)

  io.out := Mux(io.is4x4, out4x4, Mux(io.is4xN, outAsym, outSmall))
}

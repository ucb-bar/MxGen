package mxgen 

import chisel3._
import chisel3.util._


class MxExp(inA_exp_width: Int, inW_exp_width: Int, outWidth: Int, elemW: Seq[Int], outTypes: Seq[MxFormat]) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(UInt(inA_exp_width.W))
    val in_w = Input(UInt(inW_exp_width.W))
    val modeDecoded = Input(new mxMode())
    val mask_a = Input(UInt(2.W))
    val mask_w = Input(UInt(2.W))
    val enable = Input(Bool())
    val out_exp = Output(UInt(outWidth.W))
    // Per-lane SIGNED biased product exponent, BEFORE the modular truncation into out_exp. Lets
    // the consumer test underflow (post-normalization magnitude below the accumulator's subnormal
    // floor) without the wrap ambiguity that out_exp has, and flush such products to zero.
    val out_exp_signed = Output(Vec(4, SInt((outTypes.head.exp + 3).W)))
  })

  val in2_a = io.in_a.asTypeOf(Vec(2, UInt((inA_exp_width/2).W)))
  val in1_a = io.in_a.asTypeOf(Vec(1, UInt((inA_exp_width).W)))

  val in2_w = io.in_w.asTypeOf(Vec(2, UInt((inW_exp_width/2).W)))
  val in1_w = io.in_w.asTypeOf(Vec(1, UInt((inW_exp_width).W)))

  val laneMask  = VecInit((0 until 4).map(i => io.enable && (Mux(io.modeDecoded.numOutputs === 1.U, i.U < io.modeDecoded.numOutputs, Mux(io.modeDecoded.numOutputs === 2.U, Mux(io.modeDecoded.actInputs === 1.U, i.U === 0.U || i.U === 1.U, i.U === 0.U || i.U === 2.U), true.B)))))

  val adders = elemW.zipWithIndex.map{ case (w, i) =>
    val adder = Module(new AddBit(w, outTypes(i)))
    if (w > inA_exp_width/2) {
      adder.io.a := Mux(io.modeDecoded.actInputs === 1.U, in1_a(0)(w-1, 0).asSInt, in2_a(i/2).asSInt.pad(w))
    } else {
      adder.io.a := Mux(io.modeDecoded.actInputs === 1.U, in1_a(0)(w-1, 0).asSInt, in2_a(i/2)(w-1, 0).asSInt)
    }
    if (w > inW_exp_width/2) {
      adder.io.b := Mux(io.modeDecoded.weiInputs === 1.U, in1_w(0)(w-1, 0).asSInt, in2_w(i%2).asSInt.pad(w))
    } else {
      adder.io.b := Mux(io.modeDecoded.weiInputs === 1.U, in1_w(0)(w-1, 0).asSInt, in2_w(i%2)(w-1, 0).asSInt)
    }
    adder.io.enable := laneMask(i) && io.mask_a(i/2) && io.mask_w(i%2)
    adder
  }
  val sums = adders.map(_.io.y)
  io.out_exp_signed := VecInit((0 until 4).map(i =>
    if (i < adders.length) adders(i).io.signedExp else 0.S((outTypes.head.exp + 3).W)))

  io.out_exp := Mux(io.modeDecoded.numOutputs === 1.U, sums(0).pad(outWidth),
    Mux(io.modeDecoded.numOutputs === 2.U,
      Mux(io.modeDecoded.actInputs === 1.U,
        VecInit(sums(0).pad(outWidth/2), sums(1).pad(outWidth/2)).asUInt,
        VecInit(sums(0).pad(outWidth/2), sums(2).pad(outWidth/2)).asUInt),
      VecInit(sums.map(_.pad(outWidth/4))).asUInt))

}

class AddBit(elemW: Int, outType: MxFormat) extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(elemW.W))
    val b = Input(SInt(elemW.W))
    val enable = Input(Bool())
    val y = Output(UInt((outType.exp + 1).W))
    val signedExp = Output(SInt((outType.exp + 3).W))
  })
  val partial1 = io.a +& io.b
  val bias = 2.S +& outType.bias.S.pad(outType.exp + 1)
  val result = partial1 +& bias
  io.y := Mux(io.enable, result.pad(outType.exp+1).asUInt(outType.exp, 0), 0.U((outType.exp + 1).W))
  // The full signed biased product exponent (pre-truncation). Downstream subtracts the
  // normalization shift and compares against the accumulator floor to detect true underflow.
  io.signedExp := result.pad(outType.exp + 3)
}
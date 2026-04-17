package mxgen 

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// -----------------------------------------------------------------------------
// BUNDLES AND DECODING
// -----------------------------------------------------------------------------

class mxMode extends Bundle {
  val actWidth = UInt(3.W)
  val weiWidth = UInt(3.W) 
  val weiInputs = UInt(3.W)
  val actInputs = UInt(2.W)
  val shift = Vec(2, Vec(2, UInt(3.W)))
  val numOutputs = UInt(3.W)
}

class MxTypeBundle extends Bundle {
  val exp = UInt(3.W)
  val sig = UInt(3.W)
}

// -----------------------------------------------------------------------------
// MODE DECODER
// -----------------------------------------------------------------------------

object mxModeDecode {
  def litTable = VecInit(MxPEParams.allModes.map { m =>
    (new mxMode).Lit(
      _.actWidth      -> m.actWidth.U,
      _.weiWidth      -> m.weiWidth.U,
      _.weiInputs     -> m.weiInputs.U,
      _.actInputs     -> m.actInputs.U,
      _.shift(0)(0)   -> m.shift(0)(0).U, _.shift(0)(1) -> m.shift(0)(1).U,
      _.shift(1)(0)   -> m.shift(1)(0).U, _.shift(1)(1) -> m.shift(1)(1).U,
      _.numOutputs    -> m.numOutputs.U
    )
  })
  def apply(mode: UInt): mxMode = litTable(mode)
}

class MxClassifiedFp(format: MxFormat) extends Bundle {
  val isNaN = Bool()
  val isInf = Bool()
  val isZero = Bool()
  val isSub = Bool()
  val sign = UInt(1.W)
  val exp = UInt((format.exp).W)
  val sig = UInt((format.sig - 1).W)
}

object requiredPEMode {
  def apply(a: MxTypeBundle, w: MxTypeBundle): mxMode = {
    val key = Cat(a.sig, w.sig)
    val idx = MuxLookup(key, 0.U) (Seq(
      Cat(2.U(3.W), 2.U(3.W)) -> 0.U,
      Cat(2.U(3.W), 3.U(3.W)) -> 1.U,
      Cat(2.U(3.W), 4.U(3.W)) -> 2.U,
      Cat(3.U(3.W), 2.U(3.W)) -> 3.U,
      Cat(3.U(3.W), 3.U(3.W)) -> 4.U,
      Cat(3.U(3.W), 4.U(3.W)) -> 5.U,
      Cat(4.U(3.W), 2.U(3.W)) -> 6.U,
      Cat(4.U(3.W), 3.U(3.W)) -> 7.U,
      Cat(4.U(3.W), 4.U(3.W)) -> 8.U
    ))

    mxModeDecode(idx)
  }
}

object classify {
  def apply(dataType: MxFormat, in: Bits): MxClassifiedFp = {
    val width = dataType.sig + dataType.exp
    val sign = in(width - 1)
    val exp = in(width - 2, dataType.sig - 1)
    val sig = in(dataType.sig - 2, 0)

    val out = Wire(new MxClassifiedFp(dataType))

  if (dataType.sig + dataType.exp == 8) {
    val isNaN = exp.andR && sig.orR
    val isInf = if (dataType.exp == 5) exp.andR && !sig.orR else false.B
    out.isInf := isInf
    out.isNaN := isNaN
  } else {
    out.isNaN := DontCare
    out.isInf := DontCare
  }
    val isZero = (exp === 0.U) && (sig === 0.U)
    val isSub = (exp === 0.U) && (sig =/= 0.U)

    out.sig := sig(dataType.sig - 2, 0)
    out.exp := Mux(isSub, 
                   1.U(dataType.exp.W) -% dataType.bias.U(dataType.exp.W),
                   exp -% dataType.bias.U(dataType.exp.W)) -% 1.U(1.W)
    out.sign := sign
    out.isZero := isZero
    out.isSub := isSub

    out
  }
}
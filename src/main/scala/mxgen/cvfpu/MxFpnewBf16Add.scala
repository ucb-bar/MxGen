package mxgen.cvfpu

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}

// Multi-lane BF16 adder backed by cvfpu/fpnew_fma (FpFormat=FP16ALT, op=ADD).
// numPipeRegs maps to fpnew_fma's NumPipeRegs (PipeConfig=DISTRIBUTED);
// inputs are always-valid (in_valid=1 / out_ready=1).
class MxFpnewBf16Add(val numLanes: Int, val numPipeRegs: Int = 0) extends BlackBox(
  Map(
    "NumLanes"    -> IntParam(numLanes),
    "NumPipeRegs" -> IntParam(numPipeRegs)
  )
) with HasBlackBoxResource {
  override def desiredName = s"MxFpnewBf16Add"

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val a_i   = Input(UInt((numLanes * 16).W))
    val b_i   = Input(UInt((numLanes * 16).W))
    val out_o = Output(UInt((numLanes * 16).W))
  })

  addResource("/vsrc/MxFpnewBf16Add.sv")
  addResource("/vsrc/cvfpu/src/common_cells/include/common_cells/registers.svh")
  addResource("/vsrc/cvfpu/src/common_cells/src/cf_math_pkg.sv")
  addResource("/vsrc/cvfpu/src/fpnew_pkg.sv")
  addResource("/vsrc/cvfpu/src/common_cells/src/lzc.sv")
  addResource("/vsrc/cvfpu/src/fpnew_classifier.sv")
  addResource("/vsrc/cvfpu/src/fpnew_rounding.sv")
  addResource("/vsrc/cvfpu/src/fpnew_fma.sv")
}

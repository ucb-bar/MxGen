package mxgen.cvfpu

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}

// Multi-lane BF16 adder backed by cvfpu/fpnew_fma (FpFormat=FP16ALT, op=ADD).
// `numPipeRegs` directly maps to fpnew_fma's NumPipeRegs (PipeConfig=DISTRIBUTED).
// Inputs are always-valid: the wrapper ties in_valid=1 / out_ready=1.
//
// We keep cvfpu as a clean external dependency: the upstream clone at
// `cvfpu/` is the single source of truth for those sources, and we only own
// the thin wrapper in `resources/vsrc/MxFpnewBf16Add.sv`. Override the cvfpu
// location with -Dmxgen.cvfpu.dir=<path> when invoking mill if needed.
class MxFpnewBf16Add(val numLanes: Int, val numPipeRegs: Int = 0) extends BlackBox(
  Map(
    "NumLanes"    -> IntParam(numLanes),
    "NumPipeRegs" -> IntParam(numPipeRegs)
  )
) with HasBlackBoxResource with HasBlackBoxPath {
  override def desiredName = s"MxFpnewBf16Add"

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val a_i   = Input(UInt((numLanes * 16).W))
    val b_i   = Input(UInt((numLanes * 16).W))
    val out_o = Output(UInt((numLanes * 16).W))
  })

  // Our own wrapper lives under resources/.
  addResource("/vsrc/MxFpnewBf16Add.sv")

  // cvfpu sources referenced directly from the upstream clone.
  private val cvfpuDir = sys.props.getOrElse("mxgen.cvfpu.dir", "cvfpu")
  Seq(
    s"$cvfpuDir/src/common_cells/include/common_cells/registers.svh",
    s"$cvfpuDir/src/common_cells/src/cf_math_pkg.sv",
    s"$cvfpuDir/src/fpnew_pkg.sv",
    s"$cvfpuDir/src/common_cells/src/lzc.sv",
    s"$cvfpuDir/src/fpnew_classifier.sv",
    s"$cvfpuDir/src/fpnew_rounding.sv",
    s"$cvfpuDir/src/fpnew_fma.sv",
  ).foreach(addPath)
}

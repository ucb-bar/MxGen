package mxgen

import chisel3._
import mxgen.hardfloat.MxHardfloatFMA

// Elaborates MxFpMul and/or the hardfloat baseline for a set of named configs.
//
//   MxFpMul output         : generated/<name>/
//   Hardfloat baseline out : generated/hardfloat-baseline/<name>/
//
// CLI:
//   no args           -> generate both generators for every config
//   mxfpmul           -> MxFpMul only
//   hardfloat         -> hardfloat baseline only
//
// Configs available out of the box:
//   MxConfig.fp4Only                    // smallest, FP4 only
//   MxConfig.fp6 / fp8                  // FP6 or FP8 format families
//   MxConfig.mxGemmini                  // FP4/FP6_E3M2/FP8_E4M3, same-format only
//   MxConfig.all                        // every format combo (cross products)
//   MxConfig(actFormats, weiFormats)    // custom format sets

// Thin wrapper that adds input + output pipeline registers around a PE. The
// inner modules are purely combinational, so synthesis has no timing target
// without a register boundary on each end.
class RegisteredMxFpMul(config: MxConfig) extends Module {
  val accRec   = config.accFormat.recoded
  val numLanes = config.numActiveOutputLanes
  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val type_a        = Input(new MxTypeBundle())
    val in_weights    = Input(UInt(config.inWeiBusWidth.W))
    val type_w        = Input(new MxTypeBundle())
    val mode          = Input(new mxMode())
    val enable        = Input(Bool())
    val rec_c         = Input(UInt((numLanes * accRec).W))
    val out           = Output(UInt((numLanes * accRec).W))
  })
  val inner = Module(new MxFpMul(config, lut = true))
  inner.io.in_activation := RegNext(io.in_activation)
  inner.io.type_a        := RegNext(io.type_a)
  inner.io.in_weights    := RegNext(io.in_weights)
  inner.io.type_w        := RegNext(io.type_w)
  inner.io.mode          := RegNext(io.mode)
  inner.io.enable        := RegNext(io.enable)
  inner.io.rec_c         := RegNext(io.rec_c)
  io.out                 := RegNext(inner.io.out)
}

class RegisteredMxHardfloatFMA(config: MxConfig) extends Module {
  val accRec   = config.accFormat.recoded
  val numLanes = config.numActiveOutputLanes
  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val type_a        = Input(new MxTypeBundle())
    val in_weights    = Input(UInt(config.inWeiBusWidth.W))
    val type_w        = Input(new MxTypeBundle())
    val mode          = Input(new mxMode())
    val enable        = Input(Bool())
    val rec_c         = Input(UInt((numLanes * accRec).W))
    val out           = Output(UInt((numLanes * accRec).W))
  })
  val inner = Module(new MxHardfloatFMA(config))
  inner.io.in_activation := RegNext(io.in_activation)
  inner.io.type_a        := RegNext(io.type_a)
  inner.io.in_weights    := RegNext(io.in_weights)
  inner.io.type_w        := RegNext(io.type_w)
  inner.io.mode          := RegNext(io.mode)
  inner.io.enable        := RegNext(io.enable)
  inner.io.rec_c         := RegNext(io.rec_c)
  io.out                 := RegNext(inner.io.out)
}

object Main extends App {
  val configs: Seq[(String, MxConfig)] = Seq(
    "fp4-only"  -> MxConfig.fp4Only,
    "fp6-e3m2"  -> MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP6_E3M2)),
    "fp8-e4m3"  -> MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP8_E4M3)),
    "fp6"       -> MxConfig.fp6,
    "fp8"       -> MxConfig.fp8,
    "mxgemmini" -> MxConfig.mxGemmini,
    "all"       -> MxConfig.all,
    // Every format supported on both sides, but PE modes restricted to the
    // same-format pairs (FP4×FP4, FP6_E3M2×FP6_E3M2 / FP8_E5M2×FP8_E5M2,
    // FP6_E2M3×FP6_E2M3 / FP8_E4M3×FP8_E4M3). No cross-format multiplies.
    "no-cross"  -> MxConfig(
      actFormats     = MxFormat.all,
      weiFormats     = MxFormat.all,
      inActBusWidth  = MxConfig.minBusWidth(MxFormat.all),
      inWeiBusWidth  = MxConfig.minBusWidth(MxFormat.all),
      expAdderWidths = Seq(5, 5, 5, 5),
      modesOverride  = Some(List(MxPEParams.mode0, MxPEParams.mode4, MxPEParams.mode8))
    ),
  )

  val arg = args.headOption.getOrElse("both").toLowerCase
  val runMxFpMul  = arg == "both" || arg == "mxfpmul"
  val runBaseline = arg == "both" || arg == "hardfloat"
  require(runMxFpMul || runBaseline, s"Unknown argument '$arg' — expected 'mxfpmul', 'hardfloat', or 'both'")

  for ((name, config) <- configs) {
    if (runMxFpMul) {
      println(s"=== Elaborating MxFpMul: $name ===")
      println(config.describe)
      circt.stage.ChiselStage.emitSystemVerilogFile(
        new MxFpMul(config, lut = false),
        Array("--target-dir", s"generated/$name")
      )
    }
    if (runBaseline) {
      println(s"=== Elaborating hardfloat baseline: $name ===")
      circt.stage.ChiselStage.emitSystemVerilogFile(
        new MxHardfloatFMA(config),
        Array("--target-dir", s"generated/hardfloat-baseline/$name")
      )
    }
  }
}

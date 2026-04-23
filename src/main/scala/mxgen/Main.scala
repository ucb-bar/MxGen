package mxgen

import chisel3._
import mxgen.hardfloat.{MxHardfloatFMA, MxPerFormatFMA}

// Input+output register wrapper. `latency=1` forwards to the inner module and
// adds one pipeline register between the product and add stage.
class RegisteredMxFpMul(config: MxConfig, latency: Int = 0) extends Module {
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
  val inner = Module(new MxFpMul(config, lut = true, latency = latency))
  inner.io.in_activation := RegNext(io.in_activation)
  inner.io.type_a        := RegNext(io.type_a)
  inner.io.in_weights    := RegNext(io.in_weights)
  inner.io.type_w        := RegNext(io.type_w)
  inner.io.mode          := RegNext(io.mode)
  inner.io.enable        := RegNext(io.enable)
  inner.io.rec_c         := RegNext(io.rec_c)
  io.out                 := RegNext(inner.io.out)
}

class RegisteredMxHardfloatFMA(config: MxConfig, latency: Int = 0) extends Module {
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
  val inner = Module(new MxHardfloatFMA(config, latency = latency))
  inner.io.in_activation := RegNext(io.in_activation)
  inner.io.type_a        := RegNext(io.type_a)
  inner.io.in_weights    := RegNext(io.in_weights)
  inner.io.type_w        := RegNext(io.type_w)
  inner.io.mode          := RegNext(io.mode)
  inner.io.enable        := RegNext(io.enable)
  inner.io.rec_c         := RegNext(io.rec_c)
  io.out                 := RegNext(inner.io.out)
}

class RegisteredMxPerFormatFMA(config: MxConfig, latency: Int = 0) extends Module {
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
  val inner = Module(new MxPerFormatFMA(config, latency = latency))
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
    // All formats on both sides, but PE modes restricted to same-format pairs only.
    "no-cross"  -> MxConfig(
      actFormats     = MxFormat.all,
      weiFormats     = MxFormat.all,
      inActBusWidth  = MxConfig.minBusWidth(MxFormat.all),
      inWeiBusWidth  = MxConfig.minBusWidth(MxFormat.all),
      expAdderWidths = Seq(5, 5, 5, 5),
      modesOverride  = Some(List(MxPEParams.mode0, MxPEParams.mode4, MxPEParams.mode8))
    ),
  )

  val positional = args.filterNot(_.contains("="))
  val flags      = args.filter(_.contains("=")).map { s =>
    val Array(k, v) = s.split("=", 2); k.toLowerCase -> v
  }.toMap

  val arg = positional.headOption.getOrElse("all").toLowerCase
  val runAll      = arg == "all" || arg == "both"
  val runMxFpMul  = runAll || arg == "mxfpmul"
  val runBaseline = runAll || arg == "hardfloat"
  val runPerFmt   = runAll || arg == "performat"
  require(runMxFpMul || runBaseline || runPerFmt,
    s"Unknown argument '$arg' — expected 'mxfpmul', 'hardfloat', 'performat', or 'all'")

  val latency = flags.get("latency").map(_.toInt).getOrElse(0)
  require(latency == 0 || latency == 1,
    s"latency must be 0 or 1 (got $latency)")
  val latencyDir = if (latency == 0) "" else s"-lat$latency"
  println(s"=== Elaboration latency=$latency ===")

  for ((name, config) <- configs) {
    if (runMxFpMul) {
      println(s"=== Elaborating MxFpMul: $name (latency=$latency) ===")
      println(config.describe)
      circt.stage.ChiselStage.emitSystemVerilogFile(
        new MxFpMul(config, lut = false, latency = latency),
        Array("--target-dir", s"generated$latencyDir/$name")
      )
    }
    if (runBaseline) {
      println(s"=== Elaborating hardfloat baseline: $name (latency=$latency) ===")
      circt.stage.ChiselStage.emitSystemVerilogFile(
        new MxHardfloatFMA(config, latency = latency),
        Array("--target-dir", s"generated$latencyDir/hardfloat-baseline/$name")
      )
    }
    if (runPerFmt) {
      println(s"=== Elaborating per-format baseline: $name (latency=$latency) ===")
      circt.stage.ChiselStage.emitSystemVerilogFile(
        new MxPerFormatFMA(config, latency = latency),
        Array("--target-dir", s"generated$latencyDir/per-format/$name")
      )
    }
  }
}

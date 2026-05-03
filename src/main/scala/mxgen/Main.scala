package mxgen

import chisel3._
import mxgen.hardfloat.{MxHardfloatFMA, MxPerFormatFMA}

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

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
    // Same as fp8 but routes the per-lane add through cvfpu/fpnew BF16 adder.
    "fp8-fpnew" -> MxConfig.fp8.copy(useFpnewAdder = true),
    "all-fpnew" -> MxConfig.all.copy(useFpnewAdder = true)
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

  // latency=N picks one specific pipeline depth; omit to generate all of 0/1/2.
  val latencies: Seq[Int] = flags.get("latency") match {
    case Some(s) =>
      val n = s.toInt
      require(n >= 0 && n <= 2, s"latency must be 0, 1, or 2 (got $n)")
      Seq(n)
    case None => Seq(0, 1, 2)
  }
  def latencyDir(l: Int): String = if (l == 0) "" else s"-lat$l"

  // tb=0 disables emission of per-DUT Verilog testbenches (default: on).
  val runTB = flags.get("tb").map(_.toInt).getOrElse(1) != 0

  println(s"=== Elaboration latencies=${latencies.mkString(",")} tb=$runTB ===")

  // Sanitize a config label so it can be embedded in a Verilog identifier.
  def safeIdent(s: String): String = s.replaceAll("[^A-Za-z0-9_]", "_")

  def emitTB(
    name:           String,
    config:         MxConfig,
    kind:           MxPEDutKind,
    targetDir:      String,
    sameFormatOnly: Boolean,
    latency:        Int
  ): Unit = {
    val vectors = MxPETBVectors.generate(config, sameFormatOnly = sameFormatOnly)
    val tbName  = s"MxPEVerilogTB_${kind.label.replace('-', '_')}_${safeIdent(name)}" +
                  (if (latency == 0) "" else s"_lat$latency")
    println(s"=== Elaborating testbench: $tbName (${vectors.length} vectors) ===")
    circt.stage.ChiselStage.emitSystemVerilog(
      new MxPEVerilogTB(config, kind, vectors, latency = latency, desiredName = tbName),
      Array.empty,
      Array("--split-verilog", "-o", targetDir)
    )
  }

  def splitFirtoolOpts(dir: String): Array[String] =
    Array("--split-verilog", "-o", dir)


  def inlineRegistersSvh(dir: String): Unit = {
    val fmaPath  = Paths.get(dir, "fpnew_fma.sv")
    val regsPath = Paths.get(dir, "registers.svh")
    if (!Files.exists(fmaPath) || !Files.exists(regsPath)) return
    val lines = Files.readAllLines(fmaPath).asScala.toSeq
    val includeRe = """^\s*`include\s+"common_cells/registers.svh"\s*$""".r
    if (!lines.exists(l => includeRe.findFirstIn(l).isDefined)) return
    val regsContent = Files.readAllLines(regsPath).asScala.toSeq
    val patched = lines.flatMap {
      case l if includeRe.findFirstIn(l).isDefined =>
        Seq("// `include \"common_cells/registers.svh\" — inlined below for synth self-containment") ++ regsContent
      case l => Seq(l)
    }
    Files.write(fmaPath, patched.asJava)
    Files.deleteIfExists(regsPath)
  }

  for (latency <- latencies; (name, config) <- configs) {
    if (runMxFpMul) {
      println(s"=== Elaborating MxFpMul: $name (latency=$latency) ===")
      println(config.describe)
      val dir = s"generated${latencyDir(latency)}/$name"
      circt.stage.ChiselStage.emitSystemVerilog(
        new MxFpMul(config, lut = false, latency = latency),
        Array.empty,
        splitFirtoolOpts(dir)
      )
      inlineRegistersSvh(dir)
      if (runTB) {
        val tbDir = s"$dir/tb"
        emitTB(name, config, MxPEDutKind.MxFpMulDut(lut = false), tbDir, sameFormatOnly = false, latency)
        inlineRegistersSvh(tbDir)
      }
    }
    if (runBaseline) {
      println(s"=== Elaborating hardfloat baseline: $name (latency=$latency) ===")
      val dir = s"generated${latencyDir(latency)}/hardfloat-baseline/$name"
      circt.stage.ChiselStage.emitSystemVerilog(
        new MxHardfloatFMA(config, latency = latency),
        Array.empty,
        splitFirtoolOpts(dir)
      )
      if (runTB) emitTB(name, config, MxPEDutKind.HardfloatFMADut, s"$dir/tb", sameFormatOnly = false, latency)
    }
    if (runPerFmt) {
      println(s"=== Elaborating per-format baseline: $name (latency=$latency) ===")
      val dir = s"generated${latencyDir(latency)}/per-format/$name"
      circt.stage.ChiselStage.emitSystemVerilog(
        new MxPerFormatFMA(config, latency = latency),
        Array.empty,
        splitFirtoolOpts(dir)
      )
      if (runTB) emitTB(name, config, MxPEDutKind.PerFormatFMADut, s"$dir/tb", sameFormatOnly = true, latency)
    }
  }
}

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
  val runDotProd  = runAll || arg == "dotproduct"
  require(runMxFpMul || runBaseline || runPerFmt || runDotProd,
    s"Unknown argument '$arg' — expected 'mxfpmul', 'hardfloat', 'performat', 'dotproduct', or 'all'")

  // numCores=N for dot-product elaborations (1 only valid when all supported
  // PE modes emit 4 outputs).
  val dpNumCores: Int = flags.get("numcores").map(_.toInt).getOrElse(4)
  require(dpNumCores == 1 || dpNumCores == 4,
    s"numcores must be 1 or 4 (got $dpNumCores)")

  // latency=N picks one specific pipeline depth; omit to generate all of 0/1/2.
  val latencies: Seq[Int] = flags.get("latency") match {
    case Some(s) =>
      val n = s.toInt
      require(n >= 0 && n <= 2, s"latency must be 0, 1, or 2 (got $n)")
      Seq(n)
    case None => Seq(0, 1, 2)
  }
  def latencyDir(l: Int): String = if (l == 0) "" else s"-lat$l"

  // precsweep=1 sweeps prod/acc precisions across all configs at latency=1.
  val precsweep = flags.get("precsweep").exists(_ != "0")

  def splitFirtoolOpts(dir: String): Array[String] =
    Array("--split-verilog", "-o", dir)

  if (precsweep) {
    val accExps   = (4 to 8).toList
    val accMants  = List(4, 6, 8, 10, 12)
    val prodExps  = (3 to 8).toList
    val prodMants = List(3, 5, 7)
    // *-fpnew configs require accFormat=BF16(8,8) so they can't be swept.
    val sweepConfigs = configs.filterNot { case (n, _) => n.endsWith("-fpnew") }
    val precs = for {
      pe <- prodExps; pm <- prodMants
      ae <- accExps;  am <- accMants
    } yield (pe, pm, ae, am)
    val total = precs.size * sweepConfigs.size
    println(s"=== precsweep: ${precs.size} prec pairs × ${sweepConfigs.size} configs = $total per variant (latency=1) ===")
    val latency = 1
    val skipped = scala.collection.mutable.ListBuffer.empty[String]
    def trySweep(label: String)(emit: => Unit): Unit =
      try emit catch {
        case e: Throwable =>
          println(s"!!! SKIP $label — ${e.getClass.getSimpleName}: ${e.getMessage}")
          skipped += label
      }
    for ((pe, pm, ae, am) <- precs; (name, baseConfig) <- sweepConfigs) {
      val config = baseConfig.copy(
        productFormat = MxFormat.Custom(pe, pm + 1),
        accFormat     = MxFormat.Custom(ae, am + 1)
      )
      val suffix = s"pE${pe}M${pm}.accE${ae}M${am}"
      if (runMxFpMul) {
        val dir = s"generated-lat1/$name.$suffix"
        println(s"=== MxFpMul: $name.$suffix ===")
        trySweep(s"MxFpMul/$name.$suffix") {
          circt.stage.ChiselStage.emitSystemVerilog(
            new MxFpMul(config, lut = false, latency = latency),
            Array.empty, splitFirtoolOpts(dir))
        }
      }
      if (runBaseline) {
        val dir = s"generated-lat1/hardfloat-baseline/$name.$suffix"
        println(s"=== hardfloat: $name.$suffix ===")
        trySweep(s"hardfloat/$name.$suffix") {
          circt.stage.ChiselStage.emitSystemVerilog(
            new MxHardfloatFMA(config, latency = latency),
            Array.empty, splitFirtoolOpts(dir))
        }
      }
      if (runPerFmt) {
        val dir = s"generated-lat1/per-format/$name.$suffix"
        println(s"=== per-format: $name.$suffix ===")
        trySweep(s"per-format/$name.$suffix") {
          circt.stage.ChiselStage.emitSystemVerilog(
            new MxPerFormatFMA(config, latency = latency),
            Array.empty, splitFirtoolOpts(dir))
        }
      }
    }
    println(s"=== precsweep done: ${skipped.size} skipped (incompatible precision/config) ===")
    if (skipped.nonEmpty) skipped.foreach(s => println(s"    skipped: $s"))
  } else {
    println(s"=== Elaboration latencies=${latencies.mkString(",")} ===")
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
      }
      if (runBaseline) {
        println(s"=== Elaborating hardfloat baseline: $name (latency=$latency) ===")
        val dir = s"generated${latencyDir(latency)}/hardfloat-baseline/$name"
        circt.stage.ChiselStage.emitSystemVerilog(
          new MxHardfloatFMA(config, latency = latency),
          Array.empty,
          splitFirtoolOpts(dir)
        )
      }
      if (runPerFmt) {
        println(s"=== Elaborating per-format baseline: $name (latency=$latency) ===")
        val dir = s"generated${latencyDir(latency)}/per-format/$name"
        circt.stage.ChiselStage.emitSystemVerilog(
          new MxPerFormatFMA(config, latency = latency),
          Array.empty,
          splitFirtoolOpts(dir)
        )
      }
      if (runDotProd) {
        // Fall back to numCores=4 when the config has any PE mode that emits
        // fewer than 4 outputs.
        val coresFor = if (dpNumCores == 1 && !config.fixedNumOutputs.contains(4)) 4 else dpNumCores
        println(s"=== Elaborating MxDotProduct: $name (numCores=$coresFor, latency=$latency) ===")
        val dir = s"generated${latencyDir(latency)}/dot-product/$name-c$coresFor"
        circt.stage.ChiselStage.emitSystemVerilog(
          new MxDotProduct(config, lut = false, numCores = coresFor, latency = latency),
          Array.empty,
          splitFirtoolOpts(dir)
        )
      }
    }
  }
}

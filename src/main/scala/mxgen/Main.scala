package mxgen

// Elaborates an MxPE to SystemVerilog. Pick the config you want generated
// below - only the required MxPEParams mode entries will be elaborated, so
// you can trade area against format coverage directly here.
//
// Options:
//   MxConfig.fp4Only                    // smallest, FP4 only
//   MxConfig.mxGemmini                  // FP4/FP6_E3M2/FP8_E4M3
//   MxConfig.all                        // every format combo
//   MxConfig.fp6 / fp8                  // see MxParameters.scala for presets
//   MxConfig(actFormats, weiFormats)    // custom format sets
//
// You can also override `inActBusWidth`, `inWeiBusWidth`, `expAdderWidths`
// and `laneOutputWidths` per-instance.
object Main extends App {
  val config = MxConfig.mxGemmini  // or fp4Only, fp6, fp8, all (with matching bus widths)
  println(config.describe)
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new MxPE(config, lut = true),
    Array("--target-dir", "generated")
  )
}

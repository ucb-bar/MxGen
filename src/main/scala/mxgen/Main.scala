package mxgen

// Elaborates MxFpMul for a set of named configs. Each one lands in its own
// `generated/<name>/` directory so you can compare the resulting Verilog.
//
// Options available out of the box:
//   MxConfig.fp4Only                    // smallest, FP4 only
//   MxConfig.fp6 / fp8                  // FP6 or FP8 format families
//   MxConfig.mxGemmini                  // FP4/FP6_E3M2/FP8_E4M3, same-format only
//   MxConfig.all                        // every format combo (cross products)
//   MxConfig(actFormats, weiFormats)    // custom format sets
//
// You can also override `inActBusWidth`, `inWeiBusWidth`, `expAdderWidths`
// and `laneOutputWidths` per-instance.
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

  for ((name, config) <- configs) {
    println(s"=== Elaborating $name ===")
    println(config.describe)
    circt.stage.ChiselStage.emitSystemVerilogFile(
      new MxFpMul(config, lut = true),
      Array("--target-dir", s"generated/$name")
    )
  }
}

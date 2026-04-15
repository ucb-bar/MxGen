package mxgen

object Main extends App {
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new MxPE(MxParams(FPType.FP16T, FPType.FP16T), lut = true),
    Array("--target-dir", "generated")
  )
}
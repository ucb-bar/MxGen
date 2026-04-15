package mxgen

object Main extends App {
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new MxPE(MxParams(), lut = true),
    Array("--target-dir", "generated")
  )
}
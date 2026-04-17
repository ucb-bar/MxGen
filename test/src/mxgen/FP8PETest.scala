package mxgen

class MxFpMul_FP8_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig.fp8.copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8),
    expAdderWidths = Seq(5, 5, 5, 5)
  )

  def testLabel = "MxFpMul — FP8 (E4M3 + E5M2 cross-format); BF16 output; self-checking"

  def actFormats = Seq(FmtE4M3, FmtE5M2)
  def weiFormats = Seq(FmtE4M3, FmtE5M2)
}

package mxgen

class MxFpMul_MxGemmini_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig.mxGemmini.copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8)
  )

  def testLabel = "MxFpMul — mxGemmini (FP4, E3M2, E4M3 same-format); BF16 output; self-checking"

  def actFormats = Seq(FmtFP4, FmtE3M2, FmtE4M3)
  def weiFormats = Seq(FmtFP4, FmtE3M2, FmtE4M3)

  override def sameFormatOnly = true
}

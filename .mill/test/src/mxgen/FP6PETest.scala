package mxgen

class MxFpMul_FP6_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig.fp6.copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8)
  )

  def testLabel = "MxFpMul — FP6 (E2M3 + E3M2 cross-format); BF16 output; self-checking"

  def actFormats = Seq(FmtE2M3, FmtE3M2)
  def weiFormats = Seq(FmtE2M3, FmtE3M2)
}

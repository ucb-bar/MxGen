package mxgen

class MxFpMul_FP4Only_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig.fp4Only

  def testLabel = "MxFpMul — FP4 only; BF16 output; self-checking"

  def actFormats = Seq(FmtFP4)
  def weiFormats = Seq(FmtFP4)
}

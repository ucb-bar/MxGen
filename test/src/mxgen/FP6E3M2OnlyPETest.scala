package mxgen

class MxFpMul_FP6E3M2Only_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig(Set(MxFormat.FP6_E3M2), Set(MxFormat.FP6_E3M2)).copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8)
  )

  def testLabel = "MxFpMul — FP6 E3M2 only; BF16 output; self-checking"

  def actFormats = Seq(FmtE3M2)
  def weiFormats = Seq(FmtE3M2)
}

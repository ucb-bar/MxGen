package mxgen

class MxFpMul_FP8E4M3Only_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig(Set(MxFormat.FP8_E4M3), Set(MxFormat.FP8_E4M3)).copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8)
  )

  def testLabel = "MxFpMul — FP8 E4M3 only; BF16 output; self-checking"

  def actFormats = Seq(FmtE4M3)
  def weiFormats = Seq(FmtE4M3)
}

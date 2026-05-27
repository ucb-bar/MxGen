package mxgen

// Default add chain (normalize → MxPEOutToRaw → resize → MxMulAddRecFN)
// exercised at the mxGemmini config used by gemmini: productFormat=(4,4),
// accFormat=(8,8). FP4 trials match the BF16 golden bit-exactly.
class MxFpMul_DefaultPath_MxGemmini_Prod44_Acc88_Spec extends MxPETestBase {

  def testConfig = MxConfig.mxGemmini.copy(
    productFormat = MxFormat(4, 4),
    accFormat = MxFormat(8, 8)
  )

  def testLabel = "MxFpMul default path — mxGemmini productFormat=(4,4) accFormat=(8,8)"

  def actFormats = Seq(FmtFP4)
  def weiFormats = Seq(FmtFP4)

  override def sameFormatOnly = true
}

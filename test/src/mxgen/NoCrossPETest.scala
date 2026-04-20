package mxgen

class MxFpMul_NoCross_BF16Out_Spec extends MxPETestBase {

  def testConfig = MxConfig(
    actFormats     = MxFormat.all,
    weiFormats     = MxFormat.all,
    inActBusWidth  = MxConfig.minBusWidth(MxFormat.all),
    inWeiBusWidth  = MxConfig.minBusWidth(MxFormat.all),
    expAdderWidths = Seq(5, 5, 5, 5),
    modesOverride  = Some(List(MxPEParams.mode0, MxPEParams.mode4, MxPEParams.mode8))
  ).copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8)
  )

  def testLabel = "MxFpMul — all formats, same-format only (no cross); BF16 output; self-checking"

  def actFormats = Seq(FmtFP4, FmtE2M3, FmtE3M2, FmtE4M3, FmtE5M2)
  def weiFormats = Seq(FmtFP4, FmtE2M3, FmtE3M2, FmtE4M3, FmtE5M2)

  override def sameFormatOnly = true
}

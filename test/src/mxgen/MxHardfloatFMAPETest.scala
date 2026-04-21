package mxgen

// Parallel MxHardfloatFMA specs — same randomized stimulus as the MxFpMul
// tests, retargeted at the hardfloat baseline. Each spec extends its MxFpMul
// counterpart to inherit testConfig / actFormats / weiFormats / sameFormatOnly,
// then mixes in MxHardfloatFMAPETestBase to swap the DUT factory.

class MxHardfloatFMA_FP4Only_BF16Out_Spec
    extends MxFpMul_FP4Only_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — FP4 only; BF16 output; self-checking"
}

class MxHardfloatFMA_FP6E3M2Only_BF16Out_Spec
    extends MxFpMul_FP6E3M2Only_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — FP6 E3M2 only; BF16 output; self-checking"
}

class MxHardfloatFMA_FP8E4M3Only_BF16Out_Spec
    extends MxFpMul_FP8E4M3Only_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — FP8 E4M3 only; BF16 output; self-checking"
}

class MxHardfloatFMA_FP6_BF16Out_Spec
    extends MxFpMul_FP6_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — FP6 (E2M3 + E3M2 cross-format); BF16 output; self-checking"
}

class MxHardfloatFMA_FP8_BF16Out_Spec
    extends MxFpMul_FP8_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — FP8 (E4M3 + E5M2 cross-format); BF16 output; self-checking"
}

class MxHardfloatFMA_MxGemmini_BF16Out_Spec
    extends MxFpMul_MxGemmini_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — mxGemmini (FP4, E3M2, E4M3 same-format); BF16 output; self-checking"
}

class MxHardfloatFMA_NoCross_BF16Out_Spec
    extends MxFpMul_NoCross_BF16Out_Spec with MxHardfloatFMAPETestBase {
  override def testLabel = "MxHardfloatFMA — all formats, same-format only (no cross); BF16 output; self-checking"
}

// All-formats counterpart to MxFpMul_AllATypes_BF16Out_SelfChecking_NewIO_Spec
// (which is a custom inline test in FullPETest.scala). Uses MxPETestBase with
// every format on both sides — cross-format combos allowed.
class MxHardfloatFMA_AllFormats_BF16Out_Spec extends MxPETestBase with MxHardfloatFMAPETestBase {
  def testConfig = MxConfig.all.copy(
    productFormat = MxFormat(8, 8),
    accFormat = MxFormat(8, 8)
  )
  def testLabel = "MxHardfloatFMA — all formats (cross-format allowed); BF16 output; self-checking"
  def actFormats = Seq(FmtFP4, FmtE2M3, FmtE3M2, FmtE4M3, FmtE5M2)
  def weiFormats = Seq(FmtFP4, FmtE2M3, FmtE3M2, FmtE4M3, FmtE5M2)
}

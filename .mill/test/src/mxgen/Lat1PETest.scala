package mxgen

// Latency-1 counterparts of the self-checking per-config specs. Each spec
// inherits configuration (testConfig / actFormats / weiFormats / sameFormatOnly)
// from its latency-0 sibling and only overrides the pipeline latency plus a
// disambiguating label. The test loop in MxPETestBase steps `2 + latency`
// cycles per input, so the same stimulus works unchanged at latency=1.

trait PipelineLat1 extends MxPETestBase {
  override def latency: Int = 1
}

// ---- MxFpMul specs at latency=1 ----
class MxFpMul_FP4Only_BF16Out_Lat1_Spec
    extends MxFpMul_FP4Only_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — FP4 only; BF16 output; lat=1; self-checking"
}

class MxFpMul_FP6E3M2Only_BF16Out_Lat1_Spec
    extends MxFpMul_FP6E3M2Only_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — FP6 E3M2 only; BF16 output; lat=1; self-checking"
}

class MxFpMul_FP8E4M3Only_BF16Out_Lat1_Spec
    extends MxFpMul_FP8E4M3Only_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — FP8 E4M3 only; BF16 output; lat=1; self-checking"
}

class MxFpMul_FP6_BF16Out_Lat1_Spec
    extends MxFpMul_FP6_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — FP6 (E2M3 + E3M2 cross-format); BF16 output; lat=1; self-checking"
}

class MxFpMul_FP8_BF16Out_Lat1_Spec
    extends MxFpMul_FP8_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — FP8 (E4M3 + E5M2 cross-format); BF16 output; lat=1; self-checking"
}

class MxFpMul_MxGemmini_BF16Out_Lat1_Spec
    extends MxFpMul_MxGemmini_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — mxGemmini (FP4, E3M2, E4M3 same-format); BF16 output; lat=1; self-checking"
}

class MxFpMul_NoCross_BF16Out_Lat1_Spec
    extends MxFpMul_NoCross_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxFpMul — all formats, same-format only (no cross); BF16 output; lat=1; self-checking"
}

// ---- MxHardfloatFMA specs at latency=1 ----
class MxHardfloatFMA_FP4Only_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_FP4Only_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — FP4 only; BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_FP6E3M2Only_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_FP6E3M2Only_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — FP6 E3M2 only; BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_FP8E4M3Only_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_FP8E4M3Only_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — FP8 E4M3 only; BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_FP6_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_FP6_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — FP6 (E2M3 + E3M2 cross-format); BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_FP8_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_FP8_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — FP8 (E4M3 + E5M2 cross-format); BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_MxGemmini_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_MxGemmini_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — mxGemmini (FP4, E3M2, E4M3 same-format); BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_NoCross_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_NoCross_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — all formats, same-format only (no cross); BF16 output; lat=1; self-checking"
}

class MxHardfloatFMA_AllFormats_BF16Out_Lat1_Spec
    extends MxHardfloatFMA_AllFormats_BF16Out_Spec with PipelineLat1 {
  override def testLabel = "MxHardfloatFMA — all formats (cross-format allowed); BF16 output; lat=1; self-checking"
}

package mxgen

/** Mixin trait that retargets MxPETestBase's randomized test harness at
  * MxHardfloatFMA instead of MxFpMul. All poking / expected-output logic is
  * reused; only the DUT factory changes.
  *
  * Concrete hardfloat specs:
  *   class MxHardfloatFMA_FP4Only_BF16Out_Spec
  *     extends MxFpMul_FP4Only_BF16Out_Spec with MxHardfloatFMAPETestBase
  */
trait MxHardfloatFMAPETestBase extends MxPETestBase {
  /** When true, every lane uses a BF16-sized (exp=8, sig=8) multiplier; when
    * false, lanes are sized to the widest format that can flow through them. */
  def uniformBF16: Boolean = true

  override def makeHarness(config: MxConfig): Bf16OutHarnessBase =
    new MxHardfloatFMAHarnessBf16Out_NewIO(config, uniformBF16, latency = latency)
}

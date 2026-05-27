package mxgen

import chisel3._
import chisel3.util._
import hardfloat._
import mxgen.hardfloat.MxHardfloatFMA

/** Harness for MxHardfloatFMA that mirrors MxFpMulHarnessBf16Out_NewIO's IO:
  *  - Accepts BF16 raw C (E8M7, 16b), recodes to recFN(accFormat), broadcasts to lanes
  *  - Widens each lane from recFN(accFormat) to BF16 (raw and recFN), packing lane0 low
  *
  * Allows the same randomized test harness (MxPETestBase) to drive either DUT.
  */
class MxHardfloatFMAHarnessBf16Out_NewIO(config: MxConfig, uniformBF16: Boolean = true,
                                         latency: Int = 0)
    extends Bf16OutHarnessBase(config) {

  val dut = Module(new MxHardfloatFMA(config, uniformBF16, latency = latency))

  val accExp    = config.accFormat.exp
  val accSig    = config.accFormat.sig
  val accRecW   = config.accFormat.recoded
  val numActive = config.numActiveOutputLanes

  val io = IO(new Bf16OutHarnessIOBundle(config))

  val computedMode = requiredPEMode(io.type_a, io.type_w)
  dut.io.mode := computedMode

  dut.io.in_activation := io.in_activation
  dut.io.type_a        := io.type_a
  dut.io.in_weights    := io.in_weights
  dut.io.type_w        := io.type_w
  dut.io.enable        := io.enable

  // -------- C input: BF16 --> recFN(accFormat) --------
  val recCBf16 = hardfloat.recFNFromFN(8, 8, io.c_raw)
  val recCAcc: UInt = if (accExp == 8 && accSig == 8) {
    recCBf16
  } else {
    val rawC = rawFloatFromRecFN(8, 8, recCBf16)
    val cConv = Module(new RoundAnyRawFNToRecFN(8, 8, accExp, accSig, 0))
    cConv.io.in             := rawC
    cConv.io.roundingMode   := hardfloat.consts.round_near_even
    cConv.io.detectTininess := hardfloat.consts.tininess_afterRounding
    cConv.io.invalidExc     := false.B
    cConv.io.infiniteExc    := false.B
    cConv.io.out
  }

  dut.io.rec_c := Cat(Seq.fill(numActive)(recCAcc).reverse)
  io.rec_c_applied := dut.io.rec_c

  // -------- Output lanes: recFN(accFormat) --> recFN(8,8) (widen up to BF16) --------
  require(dut.io.out.getWidth == numActive * accRecW,
    s"DUT out width ${dut.io.out.getWidth} != numActiveOutputLanes*$accRecW")

  val lanesAcc = Wire(Vec(4, UInt(accRecW.W)))
  for (i <- 0 until 4) {
    if (i < numActive) {
      val hi = (i + 1) * accRecW - 1
      val lo = i * accRecW
      lanesAcc(i) := dut.io.out(hi, lo)
    } else {
      lanesAcc(i) := 0.U
    }
  }

  val lanesBf16Rec = Wire(Vec(4, UInt(17.W)))
  for (i <- 0 until 4) {
    if (accExp == 8 && accSig == 8) {
      lanesBf16Rec(i) := lanesAcc(i)
    } else {
      val rawLane = rawFloatFromRecFN(accExp, accSig, lanesAcc(i))
      val up = Module(new RoundAnyRawFNToRecFN(accExp, accSig, 8, 8, 0))
      up.io.in             := rawLane
      up.io.roundingMode   := hardfloat.consts.round_near_even
      up.io.detectTininess := hardfloat.consts.tininess_afterRounding
      up.io.invalidExc     := false.B
      up.io.infiniteExc    := false.B
      lanesBf16Rec(i) := up.io.out
    }
  }

  val lanesBF16 = Wire(Vec(4, UInt(16.W)))
  for (i <- 0 until 4) lanesBF16(i) := hardfloat.fNFromRecFN(8, 8, lanesBf16Rec(i))

  io.out_bf16  := Cat(lanesBF16.reverse)
  io.out_recfn := Cat(lanesBf16Rec.reverse)
}

package mxgen

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import mxgen.hardfloat._

import scala.util.Random

/** Zero gating must be invisible: the PE with the adder bypass (zeroGating = true) against the same PE without it,
  * both with lane gating on, compared bit for bit on every lane the mesh consumes. The stimulus leans on the cases the
  * bypass handles itself: zero operand elements (whole bus, one half, random bits), accumulator inputs that are +0, -0,
  * quiet and signalling NaN, +-Inf, subnormal and normal, in every format pair and mode, at latency 0 and 2.
  * c is driven as IEEE BF16 and converted through recFNFromFN (and a rounding step for narrower accumulate formats),
  * exactly as the mesh feeds canonical recoded values.
  */
class ZeroGatingEquiv(config: MxConfig, lut: Boolean, latency: Int) extends Module {
  val lanes = config.numActiveOutputLanes
  val recW  = config.accFormat.recoded
  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val in_weights    = Input(UInt(config.inWeiBusWidth.W))
    val type_a        = Input(new MxTypeBundle())
    val type_w        = Input(new MxTypeBundle())
    val act_lut       = Input(Bool())
    val wei_lut       = Input(Bool())
    val enable        = Input(Bool())
    val c_bf16        = Input(Vec(lanes, UInt(16.W)))
    val numOutputs    = Output(UInt(3.W))
    val out_gated     = Output(UInt((lanes * recW).W))
    val out_ref       = Output(UInt((lanes * recW).W))
    val mismatch      = Output(Bool())
  })
  val mode = requiredPEMode(io.type_a, io.type_w, io.act_lut, io.wei_lut)
  val g = Module(new MxFpMul(config.copy(laneGating = true, zeroGating = true),  lut, latency = latency))
  val r = Module(new MxFpMul(config.copy(laneGating = true, zeroGating = false), lut, latency = latency))
  val accExp = config.accFormat.exp; val accSig = config.accFormat.sig
  val recC = io.c_bf16.map { c =>
    val rec16 = recFNFromFN(8, 8, c)
    if (accExp == 8 && accSig == 8) rec16 else {
      val cv = Module(new RoundAnyRawFNToRecFN(8, 8, accExp, accSig, 0))
      cv.io.in := rawFloatFromRecFN(8, 8, rec16)
      cv.io.roundingMode := hardfloat.consts.round_near_even
      cv.io.detectTininess := hardfloat.consts.tininess_afterRounding
      cv.io.invalidExc := false.B; cv.io.infiniteExc := false.B
      cv.io.out
    }
  }
  val recCVec = Cat(recC.reverse)
  for (d <- Seq(g, r)) {
    d.io.in_activation := io.in_activation
    d.io.in_weights    := io.in_weights
    d.io.type_a        := io.type_a
    d.io.type_w        := io.type_w
    d.io.mode          := mode
    d.io.enable        := io.enable
    d.io.rec_c         := recCVec
  }
  val nOut = ShiftRegister(mode.numOutputs, latency)
  io.numOutputs := nOut
  io.out_gated  := g.io.out
  io.out_ref    := r.io.out
  // Compare values as every consumer reads them: a recoded zero (exponent top bits 000) is identified by its sign
  // alone, the remaining exponent and fraction bits are don't-care (hardfloat's own zero outputs carry leftover
  // exponent bits, and rawFloatFromRecFN / fNFromRecFN ignore them); everything else must match bit for bit.
  // Likewise an infinity (exponent top bits 110) carries leftover low exponent and fraction bits from the rounder
  // and is read by its sign alone, and a NaN (111) by its canonical quiet payload (fNFromRecFN zeroes an infinity's
  // fraction, the adders test the isInf / isNaN flags before any field).
  def canon(x: UInt): UInt = {
    val sign = x(recW - 1)
    val expTop = x(recW - 2, recW - 4)
    val tail = 0.U((recW - 4).W)
    Mux(expTop === 0.U, Cat(sign, 0.U((recW - 1).W)),
      Mux(expTop === 6.U, Cat(sign, 6.U(3.W), tail),
        Mux(expTop === 7.U, Cat(0.U(1.W), 7.U(3.W), tail), x)))
  }
  val og = g.io.out.asTypeOf(Vec(lanes, UInt(recW.W))).map(canon)
  val or = r.io.out.asTypeOf(Vec(lanes, UInt(recW.W))).map(canon)
  io.mismatch := (0 until lanes).map(i => (i.U < nOut) && (og(i) =/= or(i))).reduce(_ || _)
}

class ZeroGatingEquivSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "MxFpMul zero gating"

  def fmtType(f: MxFormat): (Int, Int) = (f.expWidth, f.sigWidth)

  /** a BF16 pattern: zeros, NaNs, infinities, subnormals, normals of varied magnitude */
  def genC(rng: Random): Int = {
    val r = rng.nextFloat()
    if (r < 0.20f) 0x0000
    else if (r < 0.32f) 0x8000
    else if (r < 0.36f) 0x7FC0 | rng.nextInt(0x40)             // quiet NaN
    else if (r < 0.38f) 0xFFC0 | rng.nextInt(0x40)             // negative quiet NaN
    else if (r < 0.40f) 0x7F81 + rng.nextInt(0x3F)             // signalling NaN
    else if (r < 0.43f) 0x7F80
    else if (r < 0.46f) 0xFF80
    else if (r < 0.52f) (rng.nextInt(2) << 15) | (1 + rng.nextInt(0x7F))   // subnormal
    else {
      val sign = rng.nextInt(2) << 15
      val exp = 0x60 + rng.nextInt(0x40)                       // 2^-31 .. 2^32
      sign | (exp << 7) | rng.nextInt(0x80)
    }
  }

  def genBus(rng: Random, width: Int): BigInt = {
    val r = rng.nextFloat()
    if (r < 0.25f) BigInt(0)
    else if (r < 0.40f) BigInt(width, rng) & ((BigInt(1) << (width / 2)) - 1)          // high half zero
    else if (r < 0.55f) BigInt(width, rng) & (((BigInt(1) << (width / 2)) - 1) << (width / 2))   // low half zero
    else BigInt(width, rng)
  }

  def run(name: String, config: MxConfig, latency: Int, vectors: Int, seed: Int): Unit = {
    val acts = config.actFormats.toSeq.map(fmtType)
    val weis = config.weiFormats.toSeq.map(fmtType)
    val quad = config.hasMode9
    it should s"match the PE without the adder bypass on every consumed lane: $name, latency $latency" in {
      test(new ZeroGatingEquiv(config, lut = false, latency = latency)) { h =>
        val rng = new Random(seed)
        val lanes = config.numActiveOutputLanes
        var checked = 0L; var mismatches = 0
        var byOut = Map[Int, Long]()
        def drive(): Unit = {
          val (ae, as) = acts(rng.nextInt(acts.length)); val (we, ws) = weis(rng.nextInt(weis.length))
          h.io.type_a.exp.poke(ae.U); h.io.type_a.sig.poke(as.U)
          h.io.type_w.exp.poke(we.U); h.io.type_w.sig.poke(ws.U)
          h.io.act_lut.poke((quad && rng.nextBoolean()).B)
          h.io.wei_lut.poke((quad && rng.nextBoolean()).B)
          h.io.enable.poke((rng.nextInt(16) != 0).B)
          h.io.in_activation.poke(genBus(rng, config.inActBusWidth).U)
          h.io.in_weights.poke(genBus(rng, config.inWeiBusWidth).U)
          for (l <- 0 until lanes) h.io.c_bf16(l).poke(genC(rng).U)
        }
        def check(): Unit = {
          val n = h.io.numOutputs.peek().litValue.toInt
          if (h.io.mismatch.peek().litToBoolean) {
            mismatches += 1
            if (mismatches <= 5) println(f"    mismatch: numOutputs $n gated=0x${h.io.out_gated.peek().litValue}%x ref=0x${h.io.out_ref.peek().litValue}%x")
          }
          byOut = byOut.updated(n, byOut.getOrElse(n, 0L) + 1); checked += 1
        }
        for (t <- 0 until vectors + latency) {
          drive()
          if (latency == 0) check()
          h.clock.step(1)
          if (latency > 0 && t >= latency) check()
        }
        println(f"  $name latency $latency: $checked vectors, mismatches on consumed lanes: $mismatches, " +
                f"by numOutputs ${byOut.toSeq.sortBy(_._1).map { case (k, v) => s"$k:$v" }.mkString(" ")}")
        assert(mismatches == 0, s"$name: $mismatches vectors differ on a consumed lane")
      }
    }
  }

  val tapeout = MxConfig.mxGemmini
  val allQuad = MxConfig.mxGemminiAll
  val e4m3    = MxConfig.e4m3Only
  val e4m3fp4 = MxConfig.e4m3SingleFp4
  for (lat <- Seq(0, 2)) {
    run("mxGemmini (tapeout PE)", tapeout, lat, 5000, 101 + lat)
    run("mxGemminiAll (quad PE)", allQuad, lat, 5000, 113 + lat)
    run("e4m3Only (quad PE)", e4m3, lat, 4000, 127 + lat)
    run("e4m3SingleFp4", e4m3fp4, lat, 4000, 131 + lat)
  }
  for (acc <- Seq(MxFormat.Custom(4, 5), MxFormat.Custom(4, 7), MxFormat.Custom(8, 8))) {
    run(s"mxGemmini acc ${acc.label}", tapeout.copy(accFormat = acc), 0, 3000, 150 + acc.sigWidth)
    run(s"mxGemminiAll acc ${acc.label}", allQuad.copy(accFormat = acc), 0, 3000, 160 + acc.sigWidth)
  }
}

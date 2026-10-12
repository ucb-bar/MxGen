package mxgen

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** Lane gating must be invisible on every lane the mesh consumes.
  *
  * Two copies of MxFpMul (laneGating on / off) get identical stimulus; the outputs are compared bit for bit on
  * lanes 0 until numOutputs, which is exactly the set Mesh.scala keeps (it zeroes the partial sums of every other
  * lane, and the accumulator write packs lanes 0, 0..1 or 0..3). Random operands, random accumulator inputs,
  * every (act, wei) format pair of the config, random LUT-enable (quad arm) where the config has mode 9, random
  * enable, at latency 0 and 2 (the pre-add and adder pipeline registers are gated too).
  */
class LaneGatingEquiv(config: MxConfig, lut: Boolean, latency: Int) extends Module {
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
    val rec_c         = Input(UInt((lanes * recW).W))
    val numOutputs    = Output(UInt(3.W))   // aligned with the outputs (delayed by the latency)
    val out_gated     = Output(UInt((lanes * recW).W))
    val out_ref       = Output(UInt((lanes * recW).W))
    val mismatch      = Output(Bool())
  })
  val mode = requiredPEMode(io.type_a, io.type_w, io.act_lut, io.wei_lut)
  val g = Module(new MxFpMul(config.copy(laneGating = true),  lut, latency = latency))
  val r = Module(new MxFpMul(config.copy(laneGating = false), lut, latency = latency))
  for (d <- Seq(g, r)) {
    d.io.in_activation := io.in_activation
    d.io.in_weights    := io.in_weights
    d.io.type_a        := io.type_a
    d.io.type_w        := io.type_w
    d.io.mode          := mode
    d.io.enable        := io.enable
    d.io.rec_c         := io.rec_c
  }
  val nOut = ShiftRegister(mode.numOutputs, latency)
  io.numOutputs := nOut
  io.out_gated  := g.io.out
  io.out_ref    := r.io.out
  val og = g.io.out.asTypeOf(Vec(lanes, UInt(recW.W)))
  val or = r.io.out.asTypeOf(Vec(lanes, UInt(recW.W)))
  io.mismatch := (0 until lanes).map(i => (i.U < nOut) && (og(i) =/= or(i))).reduce(_ || _)
}

class LaneGatingEquivSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "MxFpMul lane gating"

  def fmtType(f: MxFormat): (Int, Int) = (f.expWidth, f.sigWidth)

  def run(name: String, config: MxConfig, latency: Int, vectors: Int, seed: Int): Unit = {
    val acts = config.actFormats.toSeq.map(fmtType)
    val weis = config.weiFormats.toSeq.map(fmtType)
    val quad = config.hasMode9
    it should s"match the ungated datapath on every consumed lane: $name, latency $latency" in {
      test(new LaneGatingEquiv(config, lut = false, latency = latency)) { h =>
        val rng = new Random(seed)
        val lanes = config.numActiveOutputLanes
        val recW  = config.accFormat.recoded
        var checked = 0L
        var mismatches = 0
        var byOut = Map[Int, Long]()
        def drive(): Unit = {
          val (ae, as) = acts(rng.nextInt(acts.length))
          val (we, ws) = weis(rng.nextInt(weis.length))
          h.io.type_a.exp.poke(ae.U); h.io.type_a.sig.poke(as.U)
          h.io.type_w.exp.poke(we.U); h.io.type_w.sig.poke(ws.U)
          h.io.act_lut.poke((quad && rng.nextBoolean()).B)
          h.io.wei_lut.poke((quad && rng.nextBoolean()).B)
          h.io.enable.poke((rng.nextInt(16) != 0).B)
          h.io.in_activation.poke(BigInt(config.inActBusWidth, rng).U)
          h.io.in_weights.poke(BigInt(config.inWeiBusWidth, rng).U)
          // accumulator inputs: random recoded values, biased towards finite normals and zeros
          val c = (0 until lanes).map { _ =>
            val r = rng.nextFloat()
            if (r < 0.15f) BigInt(0) else BigInt(recW, rng)
          }
          h.io.rec_c.poke(c.zipWithIndex.map { case (v, i) => v << (i * recW) }.reduce(_ | _).U)
        }
        for (t <- 0 until vectors + latency) {
          drive()
          if (latency == 0) {
            val n = h.io.numOutputs.peek().litValue.toInt
            if (h.io.mismatch.peek().litToBoolean) mismatches += 1
            byOut = byOut.updated(n, byOut.getOrElse(n, 0L) + 1)
            checked += 1
          }
          h.clock.step(1)
          if (latency > 0 && t >= latency) {
            val n = h.io.numOutputs.peek().litValue.toInt
            if (h.io.mismatch.peek().litToBoolean) mismatches += 1
            byOut = byOut.updated(n, byOut.getOrElse(n, 0L) + 1)
            checked += 1
          }
        }
        println(f"  $name latency $latency: $checked vectors, mismatches on consumed lanes: $mismatches, " +
                f"by numOutputs ${byOut.toSeq.sortBy(_._1).map { case (k, v) => s"$k:$v" }.mkString(" ")}")
        assert(mismatches == 0, s"$name: $mismatches vectors differ on a consumed lane")
      }
    }
  }

  val tapeout = MxConfig.mxGemmini                       // 3-format PE, modes 0/4/8 (the tapeout MacUnit)
  val allQuad = MxConfig.mxGemminiAll                    // + E2M3, E5M2, mode 9 (E4M3 quad through the LUT)
  val e4m3    = MxConfig.e4m3Only                        // E4M3 single + quad
  val e4m3fp4 = MxConfig.e4m3SingleFp4                   // E4M3 single + FP4 quad, no LUT
  for (lat <- Seq(0, 2)) {
    run("mxGemmini (tapeout PE)", tapeout, lat, 4000, 11 + lat)
    run("mxGemminiAll (quad PE)", allQuad, lat, 4000, 23 + lat)
    run("e4m3Only (quad PE)", e4m3, lat, 3000, 37 + lat)
    run("e4m3SingleFp4", e4m3fp4, lat, 3000, 41 + lat)
  }
  // the same at the mesh accumulate widths used in the tapeout rows (E4S5 .. E8S8)
  for (acc <- Seq(MxFormat.Custom(4, 5), MxFormat.Custom(4, 7), MxFormat.Custom(8, 8))) {
    run(s"mxGemmini acc ${acc.label}", tapeout.copy(accFormat = acc), 0, 2000, 50 + acc.sigWidth)
    run(s"mxGemminiAll acc ${acc.label}", allQuad.copy(accFormat = acc), 0, 2000, 60 + acc.sigWidth)
  }
}

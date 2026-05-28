package mxgen

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import hardfloat._

/** Mesh-style accumulation differential.
  *
  * The single-shot differential (MxFpMul_PathDifferential_Spec) shows
  * useDefault and useMxPEAddRecFN produce bit-identical outputs across
  * the entire FP4 × BF16 input space. Yet gemmini's FP4 matmul fails
  * with the fused path. The divergence must live in something the
  * single-shot test doesn't cover.
  *
  * This harness mimics gemmini's PE.scala output-stationary accumulator
  * pattern:
  *   - One accumulator register per path.
  *   - During !valid cycles, MAC inputs are DontCare (matching PE.scala:153-154).
  *   - During valid cycles, in_activation/in_weights/c are well-defined.
  *   - c for the MAC is the current accumulator register; output feeds
  *     the accumulator back on the next cycle.
  *
  * If the two paths handle DontCare inputs differently, this should expose it.
  */
/** Inspect MxFpMulCore directly: are per-lane peMag/peExp distinct, or has
  * MxFpMulCore collapsed them for mode 0? Same inputs as the divergence
  * reproducer below.
  */
class MxFpMulCore_LanePeMag_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val cfg = MxConfig.mxGemmini.copy(
    productFormat = MxFormat.Custom(4, 4),
    accFormat     = MxFormat(8, 8),
  )

  class CoreInspect extends Module {
    val io = IO(new Bundle {
      val a       = Input(UInt(8.W))
      val w       = Input(UInt(8.W))
      val type_a  = Input(new MxTypeBundle())
      val type_w  = Input(new MxTypeBundle())
      val peMagOut = Output(Vec(4, UInt(cfg.accFormat.sig.W)))
      val peExpOut = Output(Vec(4, UInt((cfg.productFormat.exp + 1).W)))
      val peSignOut = Output(Vec(4, Bool()))
      val rawProduct = Output(UInt(cfg.outPE_width.W))
    })
    val core = Module(new MxFpMulCore(cfg, lut = false))
    core.io.in_activation := io.a
    core.io.in_weights    := io.w
    core.io.type_a        := io.type_a
    core.io.type_w        := io.type_w
    core.io.mode          := requiredPEMode(io.type_a, io.type_w)
    core.io.enable        := true.B
    for (i <- 0 until 4) {
      io.peMagOut(i)  := core.io.peMag(i)
      io.peExpOut(i)  := core.io.peExp(i)
      io.peSignOut(i) := core.io.peSign(i)
    }
    io.rawProduct := core.io.rawProduct
  }

  it should "produce distinct per-lane peMag/peExp/peSign for mixed-nibble FP4 mode-0 inputs" in {
    test(new CoreInspect) { h =>
      h.io.type_a.exp.poke(2.U); h.io.type_a.sig.poke(2.U)
      h.io.type_w.exp.poke(2.U); h.io.type_w.sig.poke(2.U)
      h.io.a.poke(0x65.U)
      h.io.w.poke(0x9E.U)
      h.clock.step(1)
      val raw = h.io.rawProduct.peek().litValue
      println(f"rawProduct = 0x$raw%X  (width=${cfg.outPE_width})")
      for (i <- 0 until 4) {
        val m = h.io.peMagOut(i).peek().litValue
        val e = h.io.peExpOut(i).peek().litValue
        val s = h.io.peSignOut(i).peek().litValue
        println(f"  lane[$i]  peMag=0x$m%02X  peExp=0x$e%X  peSign=$s")
      }
    }
  }
}

/** Tight single-cycle reproducer extracted from the mesh-accum failure.
  *
  * The previous "sweep all (a,w) byte combos × swept c" differential test
  * only exercised UNIFORM-nibble inputs (both FP4 lanes of a equal, both of w
  * equal). When a high-nibble ≠ low-nibble (which is what real gemmini matmul
  * data has), the two adder paths diverge.
  */
class MxFpMul_MixedNibble_OneShot_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val baseConfig = MxConfig.mxGemmini.copy(
    productFormat = MxFormat.Custom(4, 4),
    accFormat     = MxFormat(8, 8),
  )
  val configDefault = baseConfig.copy(useMxPEAddRecFN = false)
  val configFused   = baseConfig.copy(useMxPEAddRecFN = true)

  class TwoPath extends Module {
    val io = IO(new Bundle {
      val a       = Input(UInt(8.W))
      val w       = Input(UInt(8.W))
      val recC    = Input(UInt((4 * 17).W))
      val type_a  = Input(new MxTypeBundle())
      val type_w  = Input(new MxTypeBundle())
      val outD    = Output(UInt((4 * 17).W))
      val outF    = Output(UInt((4 * 17).W))
    })
    // mode MUST be derived from type_a/type_w via requiredPEMode — driving
    // an all-zero mxMode bundle is invalid and collapses peMag/peExp via the
    // Mux ladder inside MxFpMulCore (numOutputs=0 falls through to single-out).
    val derivedMode = requiredPEMode(io.type_a, io.type_w)
    def build(cfg: MxConfig): UInt = {
      val m = Module(new MxFpMul(cfg, lut = false, latency = 0))
      m.io.in_activation := io.a
      m.io.in_weights    := io.w
      m.io.type_a        := io.type_a
      m.io.type_w        := io.type_w
      m.io.mode          := derivedMode
      m.io.enable        := true.B
      m.io.rec_c         := io.recC
      m.io.out
    }
    io.outD := build(configDefault)
    io.outF := build(configFused)
  }

  it should "reproduce the captured (a=0x65, w=0x9E, c=recZ) divergence" in {
    test(new TwoPath) { h =>
      h.io.type_a.exp.poke(2.U); h.io.type_a.sig.poke(2.U)
      h.io.type_w.exp.poke(2.U); h.io.type_w.sig.poke(2.U)
      val recZ = BigInt("01D80", 16)
      val recAll = (recZ << 51) | (recZ << 34) | (recZ << 17) | recZ
      h.io.recC.poke(recAll.U((4 * 17).W))
      h.io.a.poke(0x65.U)
      h.io.w.poke(0x9E.U)
      h.clock.step(1)
      val d = h.io.outD.peek().litValue
      val f = h.io.outF.peek().litValue
      val laneMask = (BigInt(1) << 17) - 1
      println("Per-lane recoded outputs (a=0x65 nibbles=[5,6], w=0x9E nibbles=[E,9], c=recodedZero):")
      for (i <- 0 until 4) {
        val ld = (d >> (i * 17)) & laneMask
        val lf = (f >> (i * 17)) & laneMask
        val flag = if (ld == lf) "  " else "<-- DIFF"
        println(f"  lane[$i] default=0x$ld%05X  fused=0x$lf%05X  $flag")
      }
      assert(d == f, "useDefault and useMxPEAddRecFN diverge on this single-cycle input")
    }
  }
}

class MxFpMul_MeshAccum_Diff_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val baseConfig = MxConfig.mxGemmini.copy(
    productFormat = MxFormat.Custom(4, 4),
    accFormat     = MxFormat(8, 8),
  )
  val configDefault = baseConfig.copy(useMxPEAddRecFN = false)
  val configFused   = baseConfig.copy(useMxPEAddRecFN = true)

  class MeshAccumHarness extends Module {
    val io = IO(new Bundle {
      val a       = Input(UInt(8.W))   // 2 lanes of FP4
      val w       = Input(UInt(8.W))
      val type_a  = Input(new MxTypeBundle())
      val type_w  = Input(new MxTypeBundle())
      val valid   = Input(Bool())
      val reset_acc = Input(Bool())    // clear accumulator (mimics PE init)
      val accDefault_bf16 = Output(UInt(16.W))  // lane[0] only
      val accFused_bf16   = Output(UInt(16.W))
      // Diagnostics
      val recodedZeroBits = Output(UInt(17.W))
      val accDefaultRaw0  = Output(UInt(17.W))
      val accFusedRaw0    = Output(UInt(17.W))
      val outDefaultRaw0  = Output(UInt(17.W))
      val outFusedRaw0    = Output(UInt(17.W))
    })

    val nLanes = 4
    val laneRec = baseConfig.accFormat.recoded   // 17 bits for BF16
    // mode MUST be derived from type_a/type_w via requiredPEMode — driving
    // an all-zero mxMode bundle is invalid and collapses peMag/peExp via the
    // Mux ladder inside MxFpMulCore (numOutputs=0 falls through to single-out).
    val derivedMode = requiredPEMode(io.type_a, io.type_w)

    // Expose the recoded zero for diagnostics
    val recodedZeroDiag = recFNFromFN(8, 8, 0.U(16.W))
    io.recodedZeroBits := recodedZeroDiag

    def buildPath(cfg: MxConfig, expose: Option[(UInt, UInt)] = None): UInt = {
      val mac = Module(new MxFpMul(cfg, lut = false, latency = 0))
      // Per-lane accumulator register, recoded BF16. RegInit literal 0 is NOT
      // a valid recoded zero — convert IEEE BF16 0.0 to recFN first.
      val recodedZero = recFNFromFN(8, 8, 0.U(16.W))
      val acc = Reg(Vec(nLanes, UInt(laneRec.W)))
      when (reset.asBool) {
        for (i <- 0 until nLanes) { acc(i) := recodedZero }
      }

      // Drive MAC. Match PE.scala: in_a always driven, in_b and in_c are
      // DontCare on !valid (PE.scala:153-154).
      mac.io.in_activation := io.a
      mac.io.in_weights    := DontCare
      mac.io.rec_c         := DontCare
      when (io.valid) {
        mac.io.in_weights := io.w
        mac.io.rec_c      := acc.asUInt
      }
      mac.io.type_a        := io.type_a
      mac.io.type_w        := io.type_w
      mac.io.mode          := derivedMode
      mac.io.enable        := io.valid

      // Pull out per-lane output and feed back on valid.
      val outVec = mac.io.out.asTypeOf(Vec(nLanes, UInt(laneRec.W)))
      for (i <- 0 until nLanes) {
        when (io.reset_acc)   { acc(i) := recodedZero }
          .elsewhen (io.valid) { acc(i) := outVec(i) }
          .otherwise          { acc(i) := acc(i) }
      }

      // Expose lane[0] raw recoded acc and current MAC out for diagnostics.
      expose.foreach { case (accPort, outPort) =>
        accPort := acc(0)
        outPort := outVec(0)
      }

      // Convert lane[0] from recFN(8,8) back to IEEE BF16 for the output port.
      fNFromRecFN(8, 8, acc(0))
    }

    io.accDefault_bf16 := buildPath(configDefault, Some(io.accDefaultRaw0, io.outDefaultRaw0))
    io.accFused_bf16   := buildPath(configFused,   Some(io.accFusedRaw0,   io.outFusedRaw0))
  }

  behavior of "MxFpMul mesh-style accumulation"

  it should "match useDefault when alternating valid/invalid cycles with DontCare inputs" in {
    test(new MeshAccumHarness) { h =>

      val rng       = new Random(0xACC1DEEDL)
      val numCycles = 16

      // FP4 same-format mode 0
      h.io.type_a.exp.poke(2.U)
      h.io.type_a.sig.poke(2.U)
      h.io.type_w.exp.poke(2.U)
      h.io.type_w.sig.poke(2.U)

      // Clear both accumulators.
      h.io.reset_acc.poke(true.B)
      h.io.valid.poke(false.B)
      h.io.a.poke(0.U); h.io.w.poke(0.U)
      h.clock.step(1)
      h.io.reset_acc.poke(false.B)

      // After reset_acc step: both accs should now hold recodedZero.
      val recZBits = h.io.recodedZeroBits.peek().litValue
      val accD0 = h.io.accDefaultRaw0.peek().litValue
      val accF0 = h.io.accFusedRaw0.peek().litValue
      println(f"[init] recodedZero=0x${recZBits}%05X   accDefault[0]=0x${accD0}%05X   accFused[0]=0x${accF0}%05X")

      val mismatches = scala.collection.mutable.ListBuffer.empty[(Int, Int, Int)]

      for (t <- 0 until numCycles) {
        // ~75% valid, 25% invalid — matches the kind of valid/invalid mix
        // a real systolic schedule sees.
        val isValid = rng.nextInt(4) != 0

        if (isValid) {
          // Random FP4 a/w in both lanes
          val a = rng.nextInt(256)
          val w = rng.nextInt(256)
          h.io.a.poke(a.U)
          h.io.w.poke(w.U)
          h.io.valid.poke(true.B)
        } else {
          // Drive 0 from the test side — the harness Mux to DontCare on !valid.
          h.io.a.poke(0.U)
          h.io.w.poke(0.U)
          h.io.valid.poke(false.B)
        }

        // Sample BEFORE the clock edge so we see the MAC's combinational
        // output for the current (a, w, acc) input — that's what would be
        // written to acc on the next edge.
        val combOutD = h.io.outDefaultRaw0.peek().litValue
        val combOutF = h.io.outFusedRaw0.peek().litValue
        val combAccD = h.io.accDefaultRaw0.peek().litValue
        val combAccF = h.io.accFusedRaw0.peek().litValue

        h.clock.step(1)

        val d = h.io.accDefault_bf16.peek().litValue.toInt
        val f = h.io.accFused_bf16.peek().litValue.toInt
        if (d != f) {
          if (mismatches.isEmpty) {
            println(f"[FIRST DIVERGE] cycle=$t  valid=$isValid")
            println(f"  inputs: a=0x${h.io.a.peek().litValue}%02X  w=0x${h.io.w.peek().litValue}%02X")
            println(f"  PRE-edge  accD=0x${combAccD}%05X  accF=0x${combAccF}%05X")
            println(f"  PRE-edge  outD=0x${combOutD}%05X  outF=0x${combOutF}%05X")
            println(f"  POST-edge accD_bf16=0x$d%04X  accF_bf16=0x$f%04X")
          }
          mismatches += ((t, d, f))
        }
      }

      if (mismatches.nonEmpty) {
        println(s"[MESH-ACCUM] ${mismatches.size}/$numCycles cycles diverge")
        mismatches.take(15).foreach { case (t, d, f) =>
          println(f"  cycle $t%3d  default=0x$d%04X  fused=0x$f%04X")
        }
        fail(s"Accumulators diverge in ${mismatches.size}/$numCycles cycles")
      }
    }
  }
}

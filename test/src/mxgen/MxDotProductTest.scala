package mxgen

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import hardfloat._

// MxDotProduct harness with BF16 IEEE I/O for c and out.
class MxDotProductHarness(val config: MxConfig, val numCores: Int, val latency: Int)
    extends Module {
  val accExp  = config.accFormat.exp
  val accSig  = config.accFormat.sig
  val accRecW = config.accFormat.recoded

  val io = IO(new Bundle {
    val in_activation = Input(UInt((numCores * config.inActBusWidth).W))
    val type_a        = Input(new MxTypeBundle())
    val in_weights    = Input(UInt((numCores * config.inWeiBusWidth).W))
    val type_w        = Input(new MxTypeBundle())
    val enable        = Input(Bool())
    val accumulate    = Input(Bool())
    val c_raw         = Input(UInt(16.W))
    val out_bf16      = Output(UInt(16.W))
  })

  val dut = Module(new MxDotProduct(config, lut = false,
                                     numCores = numCores,
                                     latency = latency, coreLatency = 0))
  dut.io.mode := requiredPEMode(io.type_a, io.type_w)
  dut.io.in_activation := io.in_activation
  dut.io.type_a        := io.type_a
  dut.io.in_weights    := io.in_weights
  dut.io.type_w        := io.type_w
  dut.io.enable        := io.enable
  dut.io.accumulate    := io.accumulate

  val recCBf16 = recFNFromFN(8, 8, io.c_raw)
  val recCAcc: UInt =
    if (accExp == 8 && accSig == 8) recCBf16
    else {
      val rawC  = rawFloatFromRecFN(8, 8, recCBf16)
      val cConv = Module(new RoundAnyRawFNToRecFN(8, 8, accExp, accSig, 0))
      cConv.io.in             := rawC
      cConv.io.roundingMode   := hardfloat.consts.round_near_even
      cConv.io.detectTininess := hardfloat.consts.tininess_afterRounding
      cConv.io.invalidExc     := false.B
      cConv.io.infiniteExc    := false.B
      cConv.io.out
    }
  dut.io.rec_c := recCAcc

  val outRec17 =
    if (accExp == 8 && accSig == 8) dut.io.out
    else {
      val rawO = rawFloatFromRecFN(accExp, accSig, dut.io.out)
      val up   = Module(new RoundAnyRawFNToRecFN(accExp, accSig, 8, 8, 0))
      up.io.in             := rawO
      up.io.roundingMode   := hardfloat.consts.round_near_even
      up.io.detectTininess := hardfloat.consts.tininess_afterRounding
      up.io.invalidExc     := false.B
      up.io.infiniteExc    := false.B
      up.io.out
    }
  io.out_bf16 := fNFromRecFN(8, 8, outRec17)
}

// Self-checking MxDotProduct test against a float32 golden, with relative-error
// tolerance (BF16 output, anchor-rounded sum — no bit-exact equality).
class MxDotProduct_MxGemmini_Spec
    extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior of "MxDotProduct — mxgemmini, numCores=4, latency=0"

  it should "self-check randomized fp4 / fp6_E3M2 / fp8_E4M3 dot products" in {
    val config = MxConfig.mxGemmini.copy(productFormat = MxFormat.Custom(8, 8),
                                         accFormat     = MxFormat.Custom(8, 8))
    val numCores = 4
    val latency  = 0

    test(new MxDotProductHarness(config, numCores, latency))
        .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val expMask  = (1 << eBits) - 1
        val mantMask = (1 << mBits) - 1
        def enc(s: Int, e: Int, m: Int): Int =
          ((s & 1) << (eBits + mBits)) | ((e & expMask) << mBits) | (m & mantMask)
      }
      val FP4 = MiniFmt(2, 1, 1)
      val F62 = MiniFmt(3, 2, 3)   // fp6 E3M2
      val F84 = MiniFmt(4, 3, 7)   // fp8 E4M3

      def decode(fmt: MiniFmt, raw: Int): Float = {
        val w = fmt.eBits + fmt.mBits + 1
        val mask = (1 << w) - 1
        val rb = raw & mask
        val s = (rb >> (fmt.eBits + fmt.mBits)) & 1
        val e = (rb >> fmt.mBits) & fmt.expMask
        val m = rb & fmt.mantMask
        val mag =
          if (e == 0) {
            if (m == 0) 0.0f
            else (m.toFloat / (1 << fmt.mBits).toFloat) * math.pow(2.0, 1 - fmt.bias).toFloat
          } else (1.0f + m.toFloat / (1 << fmt.mBits).toFloat) * math.pow(2.0, e - fmt.bias).toFloat
        if (s == 1) -mag else mag
      }

      def bf16ToFloat(raw: Int): Float = java.lang.Float.intBitsToFloat((raw & 0xFFFF) << 16)
      def floatToBf16Raw(f: Float): Int = {
        val bits = java.lang.Float.floatToRawIntBits(f)
        val lsb  = (bits >>> 16) & 1
        val rnd  = bits + (0x7FFF + lsb)
        (rnd >>> 16) & 0xFFFF
      }

      def packPair(elemBits: Int, totalW: Int)(raw0: Int, raw1: Int): BigInt = {
        val half = totalW / 2
        val m = (1 << elemBits) - 1
        BigInt(raw0 & m) | (BigInt(raw1 & m) << half)
      }

      val rng = new Random(0xCAFEBABEL)
      def genSmall(fmt: MiniFmt, signed: Boolean = true): Int = {
        val s = if (signed && rng.nextBoolean()) 1 else 0
        val e = 1 + rng.nextInt(fmt.expMask - 1).max(1)
        val m = rng.nextInt(fmt.mantMask + 1)
        fmt.enc(s, e, m)
      }
      def genBf16Modest(): Int = {
        val s = if (rng.nextBoolean()) -1f else 1f
        val mag = math.pow(2.0, rng.nextInt(4) - 2).toFloat
        floatToBf16Raw(s * rng.nextFloat() * mag)
      }

      val aW = config.inActBusWidth
      val wW = config.inWeiBusWidth

      def runMode(name: String, aExp: Int, aSig: Int, wExp: Int, wSig: Int,
                  fmt: MiniFmt, dual: Boolean, elemBits: Int,
                  trials: Int): Unit = {
        println(s"\n==== mode $name ====")
        h.io.type_a.exp.poke(aExp.U); h.io.type_a.sig.poke(aSig.U)
        h.io.type_w.exp.poke(wExp.U); h.io.type_w.sig.poke(wSig.U)
        h.io.enable.poke(true.B); h.io.accumulate.poke(true.B)

        for (t <- 0 until trials) {
          // Per-core raw values: (a0, a1), (w0, w1) for dual; single elem otherwise.
          val acts = Seq.fill(numCores)(if (dual) Seq(genSmall(fmt), genSmall(fmt))
                                        else Seq(genSmall(fmt)))
          val weis = Seq.fill(numCores)(if (dual) Seq(genSmall(fmt), genSmall(fmt))
                                        else Seq(genSmall(fmt)))
          val cRaw = genBf16Modest()
          val cVal = bf16ToFloat(cRaw)

          val actBus = (0 until numCores).foldLeft(BigInt(0)) { (acc, c) =>
            val packed = if (dual) packPair(elemBits, aW)(acts(c)(0), acts(c)(1))
                         else BigInt(acts(c)(0) & ((1 << elemBits) - 1))
            acc | (packed << (c * aW))
          }
          val weiBus = (0 until numCores).foldLeft(BigInt(0)) { (acc, c) =>
            val packed = if (dual) packPair(elemBits, wW)(weis(c)(0), weis(c)(1))
                         else BigInt(weis(c)(0) & ((1 << elemBits) - 1))
            acc | (packed << (c * wW))
          }

          // Golden = c + sum over cores of (sum over (i,j) pairs of a_i * w_j).
          val golden: Float = cVal + (0 until numCores).map { c =>
            val a = acts(c).map(decode(fmt, _))
            val w = weis(c).map(decode(fmt, _))
            (for (ai <- a; wj <- w) yield ai * wj).sum
          }.sum

          h.io.in_activation.poke(actBus.U((numCores * aW).W))
          h.io.in_weights  .poke(weiBus.U((numCores * wW).W))
          h.io.c_raw       .poke(cRaw.U(16.W))
          h.clock.step(1 + latency)

          val got16   = h.io.out_bf16.peek().litValue.toInt & 0xFFFF
          val gotF    = bf16ToFloat(got16)
          val absErr  = math.abs(gotF - golden)
          // 2^-5 relative + 1e-3 absolute floor.
          val relTol  = 1.0f / 32.0f
          val absFloor = math.max(math.abs(golden), math.abs(gotF)) * relTol
          val tol     = math.max(absFloor, 1.0e-3f)
          val ok      = absErr <= tol
          val flag    = if (ok) "OK" else "FAIL"
          println(f"  trial $t%2d  golden=$golden% .6f  got=$gotF% .6f  absErr=$absErr%.4g  tol=$tol%.4g  bf16=0x$got16%04X  [$flag]")
          assert(ok, f"$name trial $t: |got - golden| = $absErr exceeds tol $tol (golden=$golden got=$gotF)")
        }
      }

      // FP4: dual-element, 4-bit per element. type_a/type_w = (exp=2, sig=2).
      runMode("fp4",        2, 2, 2, 2, FP4, dual = true,  elemBits = 4, trials = 20)
      // FP6 E3M2: dual-element, 6-bit per element. type = (exp=3, sig=3).
      runMode("fp6_E3M2",   3, 3, 3, 3, F62, dual = true,  elemBits = 6, trials = 20)
      // FP8 E4M3: single-element, 8-bit. type = (exp=4, sig=4).
      runMode("fp8_E4M3",   4, 4, 4, 4, F84, dual = false, elemBits = 8, trials = 20)

      h.io.enable.poke(false.B)
      h.clock.step(1)
    }
  }
}

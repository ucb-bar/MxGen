package mxgen.hardfloat

import chisel3._
import chisel3.util._
import mxgen.hardfloatHelper
import mxgen.MxFormat

// -----------------------------------------------------------------------------
// Baseline hardfloat FMA modules for area comparison against MxFpMul.
//
// Two flavors per format:
//   - `*MulAddBF16`              — main baseline: a single MulAddRecFN(8,8)
//                                   (true fused multiply-add at BF16).
//   - `*MulAdd(productFmt,acc)`  — parametric precision: MulRecFN at product
//                                   precision, widen, then AddRecFN at acc
//                                   precision. Two rounding steps, but product
//                                   and accumulator widths are independent.
//
// Each module performs `numLanes` parallel (a * b + c) operations, with
// `a_bits`/`b_bits` carrying the format's native packed bits and `c_rec` /
// `out_rec` carrying recoded values at the accumulation format.
//
// Design notes:
//  - "each type has its own datapath" — per-format conversion hardware is
//    generated only for the formats actually requested; each module stands on
//    its own and can be instantiated individually.
//  - "not fused" — nothing is shared across formats inside a given per-format
//    module; a format-specific converter feeds its own multiplier/adder.
//  - "don't over-instantiate" — the top-level `MxHardfloatFMA` wrapper picks
//    an instantiation strategy based on the MxConfig so that narrower formats
//    reuse the widest format's arithmetic units (size = numActiveOutputLanes).
// -----------------------------------------------------------------------------

private[hardfloat] object BaselineHelpers {
  // ---------- Native-format bit patterns → BF16 bits ----------

  // FP4 (E2M1): no inf/NaN. All-1s encoding is the finite value 6.0.
  def fp4ToBf16(in: UInt): UInt = {
    require(in.getWidth == 4)
    val sign      = in(3)
    val expIn     = in(2, 1)
    val sigIn     = in(0)
    val isZero    = (expIn === 0.U) && (sigIn === 0.U)
    val isSubnorm = (expIn === 0.U) && (sigIn === 1.U)
    val bf16ExpNormal  = (expIn +& 126.U(8.W))(7, 0)
    val bf16ExpSubnorm = 126.U(8.W)
    val bf16Exp = Mux(isZero, 0.U(8.W), Mux(isSubnorm, bf16ExpSubnorm, bf16ExpNormal))
    val bf16Sig = Mux(isSubnorm, 0.U(7.W), sigIn ## 0.U(6.W))
    sign ## bf16Exp ## bf16Sig
  }

  // FP6 E2M3: no inf/NaN. Subnormals left-normalized into a BF16 normal.
  def fp6E2M3ToBf16(in: UInt): UInt = {
    require(in.getWidth == 6)
    val sign      = in(5)
    val expIn     = in(4, 3)
    val sigIn     = in(2, 0)
    val isZero    = (expIn === 0.U) && (sigIn === 0.U)
    val isSubnorm = (expIn === 0.U) && (sigIn =/= 0.U)
    val bf16ExpNormal = (expIn +& 126.U(8.W))(7, 0)
    val lead       = PriorityEncoder(Reverse(sigIn))
    val shift      = lead +& 1.U
    val shiftedSig = ((sigIn << shift)(2, 0))
    val bf16ExpSubnorm = (127.U(8.W) -& shift)(7, 0)
    val bf16Exp = Mux(isZero, 0.U(8.W), Mux(isSubnorm, bf16ExpSubnorm, bf16ExpNormal))
    val bf16SigNormal  = sigIn ## 0.U(4.W)
    val bf16SigSubnorm = shiftedSig ## 0.U(4.W)
    val bf16Sig = Mux(isSubnorm, bf16SigSubnorm, bf16SigNormal)
    sign ## bf16Exp ## bf16Sig
  }

  // ---------- Widening helpers ----------

  def widenRecFN(in: UInt, inE: Int, inS: Int, outE: Int, outS: Int): UInt = {
    if (inE == outE && inS == outS) in
    else {
      val m = Module(new RecFNToRecFN(inE, inS, outE, outS))
      m.io.in := in
      m.io.roundingMode := consts.round_near_even
      m.io.detectTininess := consts.tininess_afterRounding
      m.io.out
    }
  }

  // ---------- MX → extended-exp IEEE intermediates ----------
  //
  // MX FP4/FP6 formats have no Inf/NaN — every bit pattern is a finite value.
  // Feeding the raw bits to recFNFromFN at the native (e,s) would alias the
  // max-exp pattern to Inf/NaN. We widen the exponent by 1 bit (mirroring the
  // FP8 → E5M3 path used for the FP8 formats) so the rebiased MX max-exp
  // stays strictly below the target's Inf/NaN exp. Subnormals are normalized
  // up into the wider normal range.

  // FP4 (E2M1) → E3M1 IEEE bits (5 bits: 1 + 3 + 1)
  def fp4ToE3M1(in: UInt): UInt = {
    require(in.getWidth == 4)
    val sign      = in(3)
    val expIn     = in(2, 1)
    val sigIn     = in(0)
    val isZero    = (expIn === 0.U) && (sigIn === 0.U)
    val isSubnorm = (expIn === 0.U) && (sigIn === 1.U)
    val expNormal  = (expIn +& 2.U)(2, 0)
    val expSubnorm = 2.U(3.W)
    val exp = Mux(isZero, 0.U(3.W), Mux(isSubnorm, expSubnorm, expNormal))
    val sig = Mux(isSubnorm, 0.U(1.W), sigIn)
    sign ## exp ## sig
  }

  // FP6 E2M3 → E4M3 IEEE bits (8 bits: 1 + 4 + 3).
  // Uses E4M3 (not E3M3) because E3M3's min normal (2^-2) can't absorb
  // FP6_E2M3's smallest subnormal (2^-3); E4M3's min normal is 2^-6.
  def fp6E2M3ToE4M3(in: UInt): UInt = {
    require(in.getWidth == 6)
    val sign      = in(5)
    val expIn     = in(4, 3)
    val sigIn     = in(2, 0)
    val isZero    = (expIn === 0.U) && (sigIn === 0.U)
    val isSubnorm = (expIn === 0.U) && (sigIn =/= 0.U)
    val expNormal  = (expIn +& 6.U)(3, 0)
    val lead       = PriorityEncoder(Reverse(sigIn))
    val shift      = lead +& 1.U
    val shiftedSig = (sigIn << shift)(2, 0)
    val expSubnorm = (7.U(4.W) -& shift)(3, 0)
    val exp = Mux(isZero, 0.U(4.W), Mux(isSubnorm, expSubnorm, expNormal))
    val sig = Mux(isSubnorm, shiftedSig, sigIn)
    sign ## exp ## sig
  }

  // FP6 E3M2 → E4M3 IEEE bits (8 bits: 1 + 4 + 3)
  def fp6E3M2ToE4M3(in: UInt): UInt = {
    require(in.getWidth == 6)
    val sign      = in(5)
    val expIn     = in(4, 2)
    val sigIn     = in(1, 0)
    val isZero    = (expIn === 0.U) && (sigIn === 0.U)
    val isSubnorm = (expIn === 0.U) && (sigIn =/= 0.U)
    val expNormal  = (expIn +& 4.U)(3, 0)
    val lead       = PriorityEncoder(Reverse(sigIn))
    val shift      = lead +& 1.U
    val shiftedSig = (sigIn << shift)(1, 0)
    val expSubnorm = (5.U(4.W) -& shift)(3, 0)
    val exp = Mux(isZero, 0.U(4.W), Mux(isSubnorm, expSubnorm, expNormal))
    val sigNormal  = sigIn       ## 0.U(1.W)
    val sigSubnorm = shiftedSig  ## 0.U(1.W)
    val sig = Mux(isSubnorm, sigSubnorm, sigNormal)
    sign ## exp ## sig
  }

  // ---------- Native-format bit patterns → recFN at target format ----------
  //
  // Uniform pattern: native MX bits → extended-exp IEEE → recFNFromFN → widen.

  def fp4ToRec(in: UInt, targetE: Int, targetS: Int): UInt = {
    val e3m1 = fp4ToE3M1(in)
    widenRecFN(recFNFromFN(3, 2, e3m1), 3, 2, targetE, targetS)
  }

  def fp6E2M3ToRec(in: UInt, targetE: Int, targetS: Int): UInt = {
    val e4m3 = fp6E2M3ToE4M3(in)
    widenRecFN(recFNFromFN(4, 4, e4m3), 4, 4, targetE, targetS)
  }

  def fp6E3M2ToRec(in: UInt, targetE: Int, targetS: Int): UInt = {
    val e4m3 = fp6E3M2ToE4M3(in)
    widenRecFN(recFNFromFN(4, 4, e4m3), 4, 4, targetE, targetS)
  }

  def fp8E4M3ToRec(in: UInt, targetE: Int, targetS: Int): UInt = {
    val e5m3 = hardfloatHelper.fp8ToE5M3(in, altfmt = false.B)
    val rec54 = recFNFromFN(5, 4, e5m3)
    widenRecFN(rec54, 5, 4, targetE, targetS)
  }

  def fp8E5M2ToRec(in: UInt, targetE: Int, targetS: Int): UInt = {
    val e5m3 = hardfloatHelper.fp8ToE5M3(in, altfmt = true.B)
    val rec54 = recFNFromFN(5, 4, e5m3)
    widenRecFN(rec54, 5, 4, targetE, targetS)
  }

  // ---------- Arithmetic primitives ----------

  // Fused multiply-add at a single (expWidth, sigWidth).
  def mulAddRecFN(aRec: UInt, bRec: UInt, cRec: UInt, e: Int, s: Int): UInt = {
    val m = Module(new MulAddRecFN(e, s))
    m.io.op := 0.U
    m.io.a := aRec
    m.io.b := bRec
    m.io.c := cRec
    m.io.roundingMode := consts.round_near_even
    m.io.detectTininess := consts.tininess_afterRounding
    m.io.out
  }

  // Recoded encoding of 1.0 at (e, s).
  def oneRecFN(e: Int, s: Int): UInt = {
    val bias  = (BigInt(1) << (e - 1)) - 1
    val fnOne = 0.U(1.W) ## bias.U(e.W) ## 0.U((s - 1).W)
    recFNFromFN(e, s, fnOne)
  }

  // Multiply at product precision, widen, add at acc precision.
  //
  // Adds are done via MulAddRecFN(aE, aS) with b = 1.0 so we compute
  // `widened * 1.0 + c`. Using a standalone AddRecFN here would be the natural
  // choice, but this hardfloat copy's AddRecFN has a corner-case bit-range bug
  // at sigWidth in {7, 8} — MulAddRecFN hits a different rounding path and
  // works. The mul-by-one is trivially optimizable in synthesis.
  def mulThenAdd(aRec: UInt, bRec: UInt, cRec: UInt,
                 pE: Int, pS: Int, aE: Int, aS: Int): UInt = {
    val mul = Module(new MulRecFN(pE, pS))
    mul.io.a := aRec
    mul.io.b := bRec
    mul.io.roundingMode := consts.round_near_even
    mul.io.detectTininess := true.B
    val widened = widenRecFN(mul.io.out, pE, pS, aE, aS)
    val fma = Module(new MulAddRecFN(aE, aS))
    fma.io.op := 0.U
    fma.io.a := widened
    fma.io.b := oneRecFN(aE, aS)
    fma.io.c := cRec
    fma.io.roundingMode := consts.round_near_even
    fma.io.detectTininess := consts.tininess_afterRounding
    fma.io.out
  }
}

// -----------------------------------------------------------------------------
// Per-format BF16 baseline FMAs. True fused multiply-add, a*b+c, all at BF16.
// -----------------------------------------------------------------------------

abstract class PerFormatFMABF16 extends Module {
  def numLanes: Int
  def fmtBits: Int
  def convert(bits: UInt): UInt   // format bits -> recFN(8,8)

  val recBits = 17
  val io = IO(new Bundle {
    val a_bits  = Input(UInt((numLanes * fmtBits).W))
    val b_bits  = Input(UInt((numLanes * fmtBits).W))
    val c_rec   = Input(UInt((numLanes * recBits).W))
    val out_rec = Output(UInt((numLanes * recBits).W))
  })

  val aLanes = io.a_bits.asTypeOf(Vec(numLanes, UInt(fmtBits.W)))
  val bLanes = io.b_bits.asTypeOf(Vec(numLanes, UInt(fmtBits.W)))
  val cLanes = io.c_rec.asTypeOf(Vec(numLanes, UInt(recBits.W)))
  val outs   = Wire(Vec(numLanes, UInt(recBits.W)))
  for (i <- 0 until numLanes) {
    outs(i) := BaselineHelpers.mulAddRecFN(convert(aLanes(i)), convert(bLanes(i)), cLanes(i), 8, 8)
  }
  io.out_rec := outs.asUInt
}

class FP4MulAddBF16(val numLanes: Int) extends PerFormatFMABF16 {
  override def desiredName = s"FP4MulAddBF16_n${numLanes}"
  val fmtBits = 4
  def convert(bits: UInt) = BaselineHelpers.fp4ToRec(bits, 8, 8)
}
class FP6E3M2MulAddBF16(val numLanes: Int) extends PerFormatFMABF16 {
  override def desiredName = s"FP6E3M2MulAddBF16_n${numLanes}"
  val fmtBits = 6
  def convert(bits: UInt) = BaselineHelpers.fp6E3M2ToRec(bits, 8, 8)
}
class FP6E2M3MulAddBF16(val numLanes: Int) extends PerFormatFMABF16 {
  override def desiredName = s"FP6E2M3MulAddBF16_n${numLanes}"
  val fmtBits = 6
  def convert(bits: UInt) = BaselineHelpers.fp6E2M3ToRec(bits, 8, 8)
}
class FP8E4M3MulAddBF16(val numLanes: Int) extends PerFormatFMABF16 {
  override def desiredName = s"FP8E4M3MulAddBF16_n${numLanes}"
  val fmtBits = 8
  def convert(bits: UInt) = BaselineHelpers.fp8E4M3ToRec(bits, 8, 8)
}
class FP8E5M2MulAddBF16(val numLanes: Int) extends PerFormatFMABF16 {
  override def desiredName = s"FP8E5M2MulAddBF16_n${numLanes}"
  val fmtBits = 8
  def convert(bits: UInt) = BaselineHelpers.fp8E5M2ToRec(bits, 8, 8)
}

// -----------------------------------------------------------------------------
// Per-format parametric-precision FMAs.
//
// productFormat controls the multiplier width; accFormat controls the
// accumulator width. They may differ — the product is widened/narrowed to the
// accumulation format between the Mul and Add stages. This is not a single
// fused multiply-add (two round steps), but it lets us bound multiplier area
// independently of accumulator precision.
// -----------------------------------------------------------------------------

abstract class PerFormatFMA extends Module {
  def numLanes:      Int
  def fmtBits:       Int
  def productFormat: MxFormat
  def accFormat:     MxFormat
  def convert(bits: UInt, e: Int, s: Int): UInt  // format bits -> recFN(e,s)

  val recBits = accFormat.recoded
  val io = IO(new Bundle {
    val a_bits  = Input(UInt((numLanes * fmtBits).W))
    val b_bits  = Input(UInt((numLanes * fmtBits).W))
    val c_rec   = Input(UInt((numLanes * recBits).W))
    val out_rec = Output(UInt((numLanes * recBits).W))
  })

  val pE = productFormat.exp; val pS = productFormat.sig
  val aE = accFormat.exp;     val aS = accFormat.sig
  val aLanes = io.a_bits.asTypeOf(Vec(numLanes, UInt(fmtBits.W)))
  val bLanes = io.b_bits.asTypeOf(Vec(numLanes, UInt(fmtBits.W)))
  val cLanes = io.c_rec.asTypeOf(Vec(numLanes, UInt(recBits.W)))
  val outs   = Wire(Vec(numLanes, UInt(recBits.W)))
  for (i <- 0 until numLanes) {
    val aRec = convert(aLanes(i), pE, pS)
    val bRec = convert(bLanes(i), pE, pS)
    outs(i) := BaselineHelpers.mulThenAdd(aRec, bRec, cLanes(i), pE, pS, aE, aS)
  }
  io.out_rec := outs.asUInt
}

class FP4MulAdd(val numLanes: Int, val productFormat: MxFormat, val accFormat: MxFormat) extends PerFormatFMA {
  override def desiredName = s"FP4MulAdd_n${numLanes}_p${productFormat.exp}_${productFormat.sig}_a${accFormat.exp}_${accFormat.sig}"
  val fmtBits = 4
  def convert(bits: UInt, e: Int, s: Int) = BaselineHelpers.fp4ToRec(bits, e, s)
}
class FP6E3M2MulAdd(val numLanes: Int, val productFormat: MxFormat, val accFormat: MxFormat) extends PerFormatFMA {
  override def desiredName = s"FP6E3M2MulAdd_n${numLanes}_p${productFormat.exp}_${productFormat.sig}_a${accFormat.exp}_${accFormat.sig}"
  val fmtBits = 6
  def convert(bits: UInt, e: Int, s: Int) = BaselineHelpers.fp6E3M2ToRec(bits, e, s)
}
class FP6E2M3MulAdd(val numLanes: Int, val productFormat: MxFormat, val accFormat: MxFormat) extends PerFormatFMA {
  override def desiredName = s"FP6E2M3MulAdd_n${numLanes}_p${productFormat.exp}_${productFormat.sig}_a${accFormat.exp}_${accFormat.sig}"
  val fmtBits = 6
  def convert(bits: UInt, e: Int, s: Int) = BaselineHelpers.fp6E2M3ToRec(bits, e, s)
}
class FP8E4M3MulAdd(val numLanes: Int, val productFormat: MxFormat, val accFormat: MxFormat) extends PerFormatFMA {
  override def desiredName = s"FP8E4M3MulAdd_n${numLanes}_p${productFormat.exp}_${productFormat.sig}_a${accFormat.exp}_${accFormat.sig}"
  val fmtBits = 8
  def convert(bits: UInt, e: Int, s: Int) = BaselineHelpers.fp8E4M3ToRec(bits, e, s)
}
class FP8E5M2MulAdd(val numLanes: Int, val productFormat: MxFormat, val accFormat: MxFormat) extends PerFormatFMA {
  override def desiredName = s"FP8E5M2MulAdd_n${numLanes}_p${productFormat.exp}_${productFormat.sig}_a${accFormat.exp}_${accFormat.sig}"
  val fmtBits = 8
  def convert(bits: UInt, e: Int, s: Int) = BaselineHelpers.fp8E5M2ToRec(bits, e, s)
}

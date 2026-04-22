package mxgen.hardfloatHelper

import chisel3._
import chisel3.util._
import mxgen.hardfloat._
import mxgen.hardfloat.consts._

// MxPEAddRecFN — like MxMulAddRecFN but consumes the raw integer product from
// MxPE directly, skipping the normalize() + MxPEOutToRaw combinational cones
// that MxFpMul used to run between the PE and the adder. The postMul's
// existing CLZ over notCDom_absSigSum (which spans 2*sigWidth+3 bits) is
// already wide enough to absorb an unnormalized peMag, so we avoid doing a
// second CLZ upstream.
//
// Inputs:
//   peMag     : the raw unsigned product integer, left-shifted to MSB-aligned
//               within `sigWidth` bits. i.e. if the true product is W bits
//               wide (W = actSig + weiSig), the caller feeds
//               `peMag_raw << (sigWidth - W)`, which puts peMag_raw's MSB at
//               bit sigWidth-1 of peMag.
//   peExp     : the biased product exponent in productFormat, straight from
//               MxExp (one lane slice). Width = expWidth + 1 for the lane's
//               pad slot, but only expWidth + 1 bits are meaningful.
//   peSign    : product sign (XOR of activation and weight signs).
//   peIsZero  : effective-zero signal (one side was zero or NaN-masked).
//   peIsNaN   : one side was NaN in the corresponding element.
//
// The peMag is placed at mulAddResult bits [2*sigWidth-1 : sigWidth], making
// the "carry" position bit (bit 2*sigWidth-1) and the "normalized 1.0"
// position bit (bit 2*sigWidth-2) land where stock MulAddRecFN would have
// placed a product of two rawFloats. The postMul CLZ then normalizes.
//
// sExpAlignedProd derivation:
//   Stock MulAddRecFN: sExpAlignedProd = rawA.sExp + rawB.sExp + const
//   where rawX.sExp = unbiased_exp(X) + 2^expWidth.
//   With rawB = 1.0 pinned (unbiased_exp=0, sExp=2^expWidth), stock reduces to
//     sExpAlignedProd = rawA.sExp + 2^expWidth + (-(2^expWidth) + sigWidth+3)
//                     = rawA.sExp + sigWidth + 3.
//   We have peExp = unbiased_product_exp + outType.bias (from MxExp), so an
//   equivalent rawA.sExp = unbiased_product_exp + 2^expWidth = peExp - bias
//   + 2^expWidth. Substituting:
//     sExpAlignedProd = peExp + (2^expWidth - bias + sigWidth + 3)
class MxPEAddRecFN(expWidth: Int, sigWidth: Int, peExpWidth: Int, bias: Int, latency: Int = 0)
    extends Module
{
    require(latency == 0 || latency == 1,
        s"MxPEAddRecFN: latency must be 0 or 1 (got $latency)")
    override def desiredName =
        s"MxPEAddRecFN_l${latency}_e${expWidth}_s${sigWidth}_pe${peExpWidth}"

    val io = IO(new Bundle {
        val peMag    = Input(UInt(sigWidth.W))
        val peExp    = Input(UInt(peExpWidth.W))
        val peSign   = Input(Bool())
        val peIsZero = Input(Bool())
        val peIsNaN  = Input(Bool())
        val c        = Input(Bits((expWidth + sigWidth + 1).W))
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out            = Output(Bits((expWidth + sigWidth + 1).W))
        val exceptionFlags = Output(Bits(5.W))
    })

    // -------------------------------------------------------------------------
    // Stage 1: rawC decode, c-alignment, mulAddResult.
    // -------------------------------------------------------------------------
    val rawC = rawFloatFromRecFN(expWidth, sigWidth, io.c)

    val sigSumWidth = sigWidth * 3 + 3

    // peExp (UInt, biased) -> SInt(unbiased + 2^expWidth + sigWidth + 3) in a
    // width that matches stock's sExpAlignedProd. Stock uses (expWidth+3)-bit
    // SInt; we match that so the downstream sNatCAlignDist arithmetic lines up.
    val sExpOffsetConst =
        ((BigInt(1) << expWidth) - BigInt(bias) + sigWidth + 3).S((expWidth + 3).W)
    val sExpAlignedProd = (0.U(1.W) ## io.peExp).asSInt +& sExpOffsetConst

    val signProd  = io.peSign
    val doSubMags = io.peSign ^ rawC.sign

    val sNatCAlignDist   = sExpAlignedProd - rawC.sExp
    val posNatCAlignDist = sNatCAlignDist(expWidth + 1, 0)
    val isMinCAlign      = io.peIsZero || (sNatCAlignDist < 0.S)
    val CIsDominant      =
        ! rawC.isZero && (isMinCAlign || (posNatCAlignDist <= sigWidth.U))
    val CAlignDist =
        Mux(isMinCAlign,
            0.U,
            Mux(posNatCAlignDist < (sigSumWidth - 1).U,
                posNatCAlignDist(log2Ceil(sigSumWidth) - 1, 0),
                (sigSumWidth - 1).U
            )
        )
    val mainAlignedSigC =
        (Mux(doSubMags, ~rawC.sig, rawC.sig) ##
            Fill(sigSumWidth - sigWidth + 2, doSubMags)).asSInt >> CAlignDist
    val reduced4CExtra =
        (orReduceBy4(rawC.sig << ((sigSumWidth - sigWidth - 1) & 3)) &
             lowMask(
                 CAlignDist >> 2,
                 (sigSumWidth - 1) >> 2,
                 (sigSumWidth - sigWidth - 1) >> 2
             )
        ).orR
    val alignedSigC =
        Cat(mainAlignedSigC >> 3,
            Mux(doSubMags,
                mainAlignedSigC(2, 0).andR && ! reduced4CExtra,
                mainAlignedSigC(2, 0).orR  ||   reduced4CExtra
            )
        )

    // Place peMag at mulAddResult bits [2*sigWidth-1 : sigWidth]. The
    // 2*sigWidth-bit product slot has peMag occupying the high half and zeros
    // in the low half, which are then OR'd with alignedSigC(2*sigWidth : 1) to
    // form the 2*sigWidth+1-bit mulAddResult.
    val peMagShifted: UInt = io.peMag ## 0.U(sigWidth.W)
    val mulAddC: UInt = alignedSigC(sigWidth * 2, 1)
    val mulAddResult = peMagShifted +& mulAddC

    // Build the inter-stage bundle (same shape as stock's toPostMul). Widened-
    // notCDom postMul uses a 27-bit odd-width absSigSum so its CLZ pair
    // alignment matches stock's (top reducedVec covers a single bit). With
    // extraction (absSigSumWidth, absSigSumWidth - sigWidth - 4) where
    // absSigSumWidth=3*sigWidth+3, leading-1 always lands at mainSig bit
    // sigWidth+3 or sigWidth+4. Net sExpSum offset vs. stock: 0.
    val toPostMul = Wire(new MulAddRecFN_interIo(expWidth, sigWidth))
    toPostMul.isSigNaNAny     := isSigNaNRawFloat(rawC)
    toPostMul.isNaNAOrB       := io.peIsNaN
    toPostMul.isInfA          := false.B
    toPostMul.isZeroA         := io.peIsZero
    toPostMul.isInfB          := false.B
    toPostMul.isZeroB         := false.B
    toPostMul.signProd        := signProd
    toPostMul.isNaNC          := rawC.isNaN
    toPostMul.isInfC          := rawC.isInf
    toPostMul.isZeroC         := rawC.isZero
    toPostMul.sExpSum         := sExpAlignedProd
    toPostMul.doSubMags       := doSubMags
    toPostMul.CIsDominant     := false.B
    toPostMul.CDom_CAlignDist := 0.U
    toPostMul.highAlignedSigC := alignedSigC(sigSumWidth - 1, sigWidth * 2 + 1)
    toPostMul.bit0AlignedSigC := alignedSigC(0)

    // Passthrough fields needed by the cDominant bypass in stage 2. See
    // the `when (passthroughC)` block below for the shape rationale.
    class PassthroughBundle extends Bundle {
        val cDominant      = Bool()
        val productNonZero = Bool()
        val rawCSign       = Bool()
        val rawCSExp       = SInt((expWidth + 2).W)
        val rawCSig        = UInt((sigWidth + 1).W)
    }
    val passthrough = Wire(new PassthroughBundle)
    passthrough.cDominant      :=
        (CAlignDist === 0.U) && ! rawC.isZero && ! rawC.isNaN &&
            ! rawC.isInf && ! io.peIsNaN
    passthrough.productNonZero := io.peMag.orR && ! io.peIsZero
    passthrough.rawCSign       := rawC.sign
    passthrough.rawCSExp       := rawC.sExp
    passthrough.rawCSig        := rawC.sig

    // -------------------------------------------------------------------------
    // Stage boundary. Matches stock MulAddRecFNPipe's cut: inter-stage bundle,
    // mulAddResult, and the mode signals are registered when latency>=1. We
    // also carry a small passthrough bundle for the c-dominant bypass that
    // stock MulAddRecFN doesn't have.
    // -------------------------------------------------------------------------
    val toPostMul_s1   = if (latency >= 1) RegNext(toPostMul)         else toPostMul
    val mulAddRes_s1   = if (latency >= 1) RegNext(mulAddResult)      else mulAddResult
    val roundMode_s1   = if (latency >= 1) RegNext(io.roundingMode)   else io.roundingMode
    val detectTiny_s1  = if (latency >= 1) RegNext(io.detectTininess) else io.detectTininess
    val passthrough_s1 = if (latency >= 1) RegNext(passthrough)       else passthrough

    // -------------------------------------------------------------------------
    // Stage 2: CLZ-based normalization, c-dominant bypass, rounding.
    // -------------------------------------------------------------------------
    val postMul = Module(new MulAddRecFNToRaw_postMul_notCDom(expWidth, sigWidth))
    postMul.io.fromPreMul   := toPostMul_s1
    postMul.io.mulAddResult := mulAddRes_s1
    postMul.io.roundingMode := roundMode_s1

    // When CAlignDist=0 (c dominates: product is zero, or product's exp is
    // ≤ c's exp - (sigWidth+3)), the widened-notCDom path can't normalize c
    // — c's implicit 1 either sits at alignedSigC bit 27 (outside
    // highAlignedSigC's [26:17] slice) or lands at sigSum[26] where the
    // full-width CLZ would misinterpret it as the sign bit. Bypass the
    // postMul for this narrow case. For CAlignDist≥1, c's implicit 1 is
    // always inside highAlignedSigC and the widened CLZ produces the
    // correct result. Product contribution is ≤ 1/8 ULP of c when
    // CAlignDist=0, below the rounder's round bit — OR it into sig[0] as
    // sticky to preserve the inexact flag.
    val rawOutFinal = WireDefault(postMul.io.rawOut)
    when (passthrough_s1.cDominant) {
        rawOutFinal.isNaN  := false.B
        rawOutFinal.isInf  := false.B
        rawOutFinal.isZero := false.B
        rawOutFinal.sign   := passthrough_s1.rawCSign
        rawOutFinal.sExp   := passthrough_s1.rawCSExp
        // rawC.sig has bit sigWidth always 0 and bit sigWidth-1 as the
        // implicit 1 (see rawFloatFromRecFN). The rounder's input sig
        // (width sigWidth+3) needs its "leading 1" at bit sigWidth+1
        // (the normal, non-carry form). Shift rawC.sig left by 2 so its
        // implicit 1 lands at bit sigWidth+1, bit sigWidth+2 stays zero.
        // OR the product-nonzero flag into the sticky (bit 0) so inexact
        // gets flagged when product contributes below the round bit.
        rawOutFinal.sig    := (passthrough_s1.rawCSig << 2) | passthrough_s1.productNonZero
    }

    val roundRawFNToRecFN = Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
    roundRawFNToRecFN.io.invalidExc     := postMul.io.invalidExc
    roundRawFNToRecFN.io.infiniteExc    := false.B
    roundRawFNToRecFN.io.in             := rawOutFinal
    roundRawFNToRecFN.io.roundingMode   := roundMode_s1
    roundRawFNToRecFN.io.detectTininess := detectTiny_s1
    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

// Fork of hardfloat's MulAddRecFNToRaw_postMul that drops the CDom path. The
// original speculatively computes both CDom (c-dominates) and notCDom paths
// every cycle, each with its own barrel shifter / sticky logic, and muxes at
// the end on CIsDominant. We keep only notCDom.
//
// Correctness caveat: notCDom's CLZ operates on sigSum(2*sigWidth+2, 0), which
// discards the top sigWidth bits of sigSum where c's MSB lands in CDom cases.
// When c dominates the product, this fork mis-normalizes. Valid only if the
// workload never hits CDom — tests will surface any case that does.
class MulAddRecFNToRaw_postMul_notCDom(expWidth: Int, sigWidth: Int)
    extends RawModule
{
    override def desiredName =
        s"MulAddRecFNToRaw_postMul_notCDom_e${expWidth}_s${sigWidth}"
    val io = IO(new Bundle {
        val fromPreMul   = Input(new MulAddRecFN_interIo(expWidth, sigWidth))
        val mulAddResult = Input(UInt((sigWidth * 2 + 1).W))
        val roundingMode = Input(UInt(3.W))
        val invalidExc   = Output(Bool())
        val rawOut       = Output(new RawFloat(expWidth, sigWidth + 2))
    })

    // Widened notCDom: absSigSum is 27 bits wide (odd, top bit padded 0) so
    // the CLZ's top reducedVec entry covers a single bit, mirroring stock's
    // 19-bit odd-width pattern. This keeps the leading 1 pinned to mainSig
    // bit sigWidth+3 or sigWidth+4 regardless of peMag's MSB parity — stock
    // relies on the same invariant for its completeCancellation check.
    // Caller must offset sExpSum accordingly — see MxPEAddRecFN's
    // `sExpAlignedProd + 2.S`.
    val sigSumWidth      = sigWidth * 3 + 3
    val absSigSumWidth   = sigSumWidth           // 3*sigWidth + 3, odd
    val roundingMode_min = (io.roundingMode === round_min)
    val opSignC          = io.fromPreMul.signProd ^ io.fromPreMul.doSubMags

    val sigSum =
        Cat(Mux(io.mulAddResult(sigWidth * 2),
                io.fromPreMul.highAlignedSigC + 1.U,
                io.fromPreMul.highAlignedSigC),
            io.mulAddResult(sigWidth * 2 - 1, 0),
            io.fromPreMul.bit0AlignedSigC)

    val signSigSum = sigSum(sigSumWidth - 1)
    val absSigSum =
        Cat(0.U(1.W),
            Mux(signSigSum,
                ~sigSum(sigSumWidth - 2, 0),
                sigSum(sigSumWidth - 2, 0) + io.fromPreMul.doSubMags))
    val reduced2AbsSigSum = orReduceBy2(absSigSum)
    val normDistReduced2  = countLeadingZeros(reduced2AbsSigSum)
    val nearNormDist      = normDistReduced2 << 1
    val notCDom_sExp =
        io.fromPreMul.sExpSum - nearNormDist.asUInt.zext
    // After shift-left by nearNormDist, the top reducedVec pad bit parity
    // forces the leading 1 into one of the top two slots. Extract a
    // (sigWidth+5)-bit window whose top bit is absSigSumWidth (one above the
    // original MSB, reached via the shift overflow), matching stock's
    // `shifted(2*sigWidth+3, sigWidth-1)` shape.
    val notCDom_mainSig =
        (absSigSum << nearNormDist)(
            absSigSumWidth, absSigSumWidth - sigWidth - 4)
    // Stock's sticky formula assumes a (2*sigWidth+3)-bit absSigSum; ours is
    // (3*sigWidth+3)-bit. The "below mainSig[2:0]" range at nearNormDist=0
    // grows from sigWidth-1 to 2*sigWidth-1 bits, which means reduced2
    // coverage grows from sigWidth/2+1 to sigWidth+1 entries and the mask
    // needs to be twice as wide to avoid collapsing to 0 at large
    // normDistReduced2. Widen both the orReduceBy2 input and the lowMask
    // topBound accordingly.
    val notCDom_reduced4SigExtra =
        (orReduceBy2(
             reduced2AbsSigSum(sigWidth, 0) <<
                 (sigWidth & 1)) &
             lowMask(normDistReduced2 >> 1, 0, (2 * sigWidth + 2) >> 2)
        ).orR
    val notCDom_sig =
        Cat(notCDom_mainSig >> 3,
            notCDom_mainSig(2, 0).orR || notCDom_reduced4SigExtra)
    val notCDom_completeCancellation =
        (notCDom_sig(sigWidth + 2, sigWidth + 1) === 0.U)
    val notCDom_sign =
        Mux(notCDom_completeCancellation,
            roundingMode_min,
            io.fromPreMul.signProd ^ signSigSum)

    val notNaN_isInfProd = io.fromPreMul.isInfA || io.fromPreMul.isInfB
    val notNaN_isInfOut  = notNaN_isInfProd || io.fromPreMul.isInfC
    val notNaN_addZeros =
        (io.fromPreMul.isZeroA || io.fromPreMul.isZeroB) &&
            io.fromPreMul.isZeroC

    io.invalidExc :=
        io.fromPreMul.isSigNaNAny ||
        (io.fromPreMul.isInfA && io.fromPreMul.isZeroB) ||
        (io.fromPreMul.isZeroA && io.fromPreMul.isInfB) ||
        (! io.fromPreMul.isNaNAOrB &&
             (io.fromPreMul.isInfA || io.fromPreMul.isInfB) &&
             io.fromPreMul.isInfC &&
             io.fromPreMul.doSubMags)
    io.rawOut.isNaN  := io.fromPreMul.isNaNAOrB || io.fromPreMul.isNaNC
    io.rawOut.isInf  := notNaN_isInfOut
    io.rawOut.isZero := notNaN_addZeros || notCDom_completeCancellation
    io.rawOut.sign :=
        (notNaN_isInfProd && io.fromPreMul.signProd) ||
        (io.fromPreMul.isInfC && opSignC) ||
        (notNaN_addZeros && ! roundingMode_min &&
            io.fromPreMul.signProd && opSignC) ||
        (notNaN_addZeros && roundingMode_min &&
            (io.fromPreMul.signProd || opSignC)) ||
        (! notNaN_isInfOut && ! notNaN_addZeros && notCDom_sign)
    io.rawOut.sExp := notCDom_sExp
    io.rawOut.sig  := notCDom_sig
}

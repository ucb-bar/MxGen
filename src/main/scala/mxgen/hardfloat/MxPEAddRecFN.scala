package mxgen.hardfloatHelper

import chisel3._
import chisel3.util._
import mxgen.hardfloat._
import mxgen.hardfloat.consts._

// MxPEAddRecFN — consumes the raw integer product from MxPE directly.
// peMag is MSB-aligned within sigWidth and placed at mulAddResult[2s-1:s].
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

    // Stage 1: rawC decode, c-alignment, mulAddResult.
    val rawC = rawFloatFromRecFN(expWidth, sigWidth, io.c)

    val sigSumWidth = sigWidth * 3 + 3

    // peExp (biased UInt) → SInt matching stock's (expWidth+3)-bit sExpAlignedProd.
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

    // Place peMag at mulAddResult[2*sigWidth-1:sigWidth], then add alignedSigC.
    val peMagShifted: UInt = io.peMag ## 0.U(sigWidth.W)
    val mulAddC: UInt = alignedSigC(sigWidth * 2, 1)
    val mulAddResult = peMagShifted +& mulAddC

    // Inter-stage bundle (stock's toPostMul shape). Widened-notCDom uses a
    // 3*sigWidth+3-bit absSigSum so leading-1 pins to mainSig bits [s+3..s+4].
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

    // Stage boundary (matches stock MulAddRecFNPipe's cut, plus a small
    // passthrough bundle for the c-dominant bypass we add below).
    val toPostMul_s1   = if (latency >= 1) RegNext(toPostMul)         else toPostMul
    val mulAddRes_s1   = if (latency >= 1) RegNext(mulAddResult)      else mulAddResult
    val roundMode_s1   = if (latency >= 1) RegNext(io.roundingMode)   else io.roundingMode
    val detectTiny_s1  = if (latency >= 1) RegNext(io.detectTininess) else io.detectTininess
    val passthrough_s1 = if (latency >= 1) RegNext(passthrough)       else passthrough

    // Stage 2: CLZ-based normalization, c-dominant bypass, rounding.
    val postMul = Module(new MulAddRecFNToRaw_postMul_notCDom(expWidth, sigWidth))
    postMul.io.fromPreMul   := toPostMul_s1
    postMul.io.mulAddResult := mulAddRes_s1
    postMul.io.roundingMode := roundMode_s1

    // CAlignDist=0 bypass: widened-notCDom can't normalize c here, so pass
    // c through directly. Product contribution is sticky-only in this case.
    val rawOutFinal = WireDefault(postMul.io.rawOut)
    when (passthrough_s1.cDominant) {
        rawOutFinal.isNaN  := false.B
        rawOutFinal.isInf  := false.B
        rawOutFinal.isZero := false.B
        rawOutFinal.sign   := passthrough_s1.rawCSign
        rawOutFinal.sExp   := passthrough_s1.rawCSExp
        // Shift rawC.sig<<2 so implicit 1 lands at rounder-input bit sigWidth+1
        // (non-carry form); OR productNonZero into sticky for inexact flag.
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

// Fork of MulAddRecFNToRaw_postMul with only the notCDom path. Correctness
// caveat: mis-normalizes when c dominates — only valid if workload avoids CDom.
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

    // Widened notCDom: absSigSum is 3*sigWidth+3 bits (odd) so leading-1 pins
    // to mainSig[s+3..s+4]. Caller must offset sExpSum (see MxPEAddRecFN).
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
    // Extract a (sigWidth+5)-bit window from the shifted absSigSum whose top
    // bit is absSigSumWidth, matching stock's shifted(2s+3, s-1) shape.
    val notCDom_mainSig =
        (absSigSum << nearNormDist)(
            absSigSumWidth, absSigSumWidth - sigWidth - 4)
    // Sticky widened for our 3s+3-bit absSigSum: doubled reduced2 coverage
    // and lowMask topBound to avoid collapsing at large normDistReduced2.
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

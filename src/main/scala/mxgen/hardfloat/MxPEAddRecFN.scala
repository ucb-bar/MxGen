package mxgen.hardfloatHelper

import chisel3._
import chisel3.util._
import mxgen.hardfloat._

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
class MxPEAddRecFN(expWidth: Int, sigWidth: Int, peExpWidth: Int, bias: Int)
    extends RawModule
{
    override def desiredName =
        s"MxPEAddRecFN_e${expWidth}_s${sigWidth}_pe${peExpWidth}"

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

    val postMul = Module(new MulAddRecFNToRaw_postMul(expWidth, sigWidth))
    postMul.io.fromPreMul.isSigNaNAny     := isSigNaNRawFloat(rawC)
    postMul.io.fromPreMul.isNaNAOrB       := io.peIsNaN
    postMul.io.fromPreMul.isInfA          := false.B
    postMul.io.fromPreMul.isZeroA         := io.peIsZero
    postMul.io.fromPreMul.isInfB          := false.B
    postMul.io.fromPreMul.isZeroB         := false.B
    postMul.io.fromPreMul.signProd        := signProd
    postMul.io.fromPreMul.isNaNC          := rawC.isNaN
    postMul.io.fromPreMul.isInfC          := rawC.isInf
    postMul.io.fromPreMul.isZeroC         := rawC.isZero
    postMul.io.fromPreMul.sExpSum         :=
        Mux(CIsDominant, rawC.sExp, sExpAlignedProd - sigWidth.S)
    postMul.io.fromPreMul.doSubMags       := doSubMags
    postMul.io.fromPreMul.CIsDominant     := CIsDominant
    postMul.io.fromPreMul.CDom_CAlignDist := CAlignDist(log2Ceil(sigWidth + 1) - 1, 0)
    postMul.io.fromPreMul.highAlignedSigC := alignedSigC(sigSumWidth - 1, sigWidth * 2 + 1)
    postMul.io.fromPreMul.bit0AlignedSigC := alignedSigC(0)

    postMul.io.mulAddResult := mulAddResult
    postMul.io.roundingMode := io.roundingMode

    val roundRawFNToRecFN = Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
    roundRawFNToRecFN.io.invalidExc     := postMul.io.invalidExc
    roundRawFNToRecFN.io.infiniteExc    := false.B
    roundRawFNToRecFN.io.in             := postMul.io.rawOut
    roundRawFNToRecFN.io.roundingMode   := io.roundingMode
    roundRawFNToRecFN.io.detectTininess := io.detectTininess
    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

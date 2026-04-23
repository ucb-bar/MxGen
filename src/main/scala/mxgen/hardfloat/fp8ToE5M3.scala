package mxgen.hardfloatHelper

import chisel3._
import chisel3.util._
import mxgen.hardfloat._

// MxMulAddRecFN — MulAdd with rawB pinned to 1.0 (result = rawA + rec_c),
// using the FMA preMul. rawA is a RawFloat at (aE,aS) ≤ (expWidth,sigWidth).
class MxMulAddRecFN(expWidth: Int, sigWidth: Int,
                    aExpWidth: Int = -1, aSigWidth: Int = -1) extends RawModule
{
    val aE = if (aExpWidth < 0) expWidth else aExpWidth
    val aS = if (aSigWidth < 0) sigWidth else aSigWidth
    require(aE <= expWidth && aS <= sigWidth,
        s"MxMulAddRecFN: rawA widths (aE=$aE, aS=$aS) must be <= FMA widths " +
        s"(expWidth=$expWidth, sigWidth=$sigWidth)")

    override def desiredName =
        if (aE == expWidth && aS == sigWidth)
            s"MxMulAddRecFN_e${expWidth}_s${sigWidth}"
        else
            s"MxMulAddRecFN_e${expWidth}_s${sigWidth}_ae${aE}_as${aS}"

    val io = IO(new Bundle {
        val a = Input(new RawFloat(aE, aS))
        val c = Input(Bits((expWidth + sigWidth + 1).W))
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out = Output(Bits((expWidth + sigWidth + 1).W))
        val exceptionFlags = Output(Bits(5.W))
    })

    val rawC = rawFloatFromRecFN(expWidth, sigWidth, io.c)

    // Inline MulAddRecFNToRaw_preMul with b=1.0; sExpAlignedProd absorbs the
    // rawA→acc bias shift into the usual +sigWidth+3 offset.
    val sigSumWidth = sigWidth * 3 + 3

    val sExpAlignedProdConst =
        ((BigInt(1) << expWidth) - (BigInt(1) << aE) + sigWidth + 3).S((expWidth + 3).W)
    val sExpAlignedProd = io.a.sExp +& sExpAlignedProdConst

    val signProd  = io.a.sign
    val doSubMags = io.a.sign ^ rawC.sign

    val sNatCAlignDist   = sExpAlignedProd - rawC.sExp
    val posNatCAlignDist = sNatCAlignDist(expWidth + 1, 0)
    val isMinCAlign      = io.a.isZero || (sNatCAlignDist < 0.S)
    val CIsDominant      = ! rawC.isZero && (isMinCAlign || (posNatCAlignDist <= sigWidth.U))
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

    // mulAddA: pad rawA.sig's (aS+1)-bit field to sigWidth bits, with the
    // leading '1' at position sigWidth-1 (matching stock preMul's convention).
    val mulAddA: UInt =
        if (sigWidth == aS) io.a.sig(aS - 1, 0)
        else io.a.sig(aS - 1, 0) ## 0.U((sigWidth - aS).W)
    val mulAddB: UInt = (BigInt(1) << (sigWidth - 1)).U(sigWidth.W)
    val mulAddC: UInt = alignedSigC(sigWidth * 2, 1)

    // CIRCT folds this into `mulAddA << (sigWidth-1)` — no multiplier.
    val mulAddResult = (mulAddA * mulAddB) +& mulAddC

    val postMul = Module(new MulAddRecFNToRaw_postMul(expWidth, sigWidth))
    postMul.io.fromPreMul.isSigNaNAny :=
        isSigNaNRawFloat(io.a) || isSigNaNRawFloat(rawC)
    postMul.io.fromPreMul.isNaNAOrB       := io.a.isNaN
    postMul.io.fromPreMul.isInfA          := io.a.isInf
    postMul.io.fromPreMul.isZeroA         := io.a.isZero
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



object fp8ToE5M3 {

	def apply(in: Bits, altfmt: Bool) = {
		Mux(altfmt, { // E5M2
			in ## 0.U(1.W)
		}, { // E4M3
			val sign = in(7)
			val exp = in(6, 3)
			val sig = in(2, 0)
			val isNaN = exp.andR && sig.andR
			val isSubnormal = !exp.orR
			val subnormShift = PriorityEncoder(Reverse(sig))
			val subnormSig = ((sig << 1.U) << subnormShift)(2, 0)
			val subnormExp = 8.U(5.W) - subnormShift
			Mux(isNaN,
				sign ## "b11111100".U(8.W), // NaN
				Mux(isSubnormal,
					Mux(sig.orR,
						sign ## subnormExp ## subnormSig, // Subnormal
						sign ## "b00000000".U(8.W) // Zero
					),
					sign ## ((0.U(1.W) ## exp) + 8.U) ## sig // Normal
				)
			)
		})
	}
}

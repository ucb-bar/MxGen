package mxgen

import chisel3._
import chisel3.util._
import mxgen.hardfloat._
import mxgen.hardfloat.consts._

/** Anchor-aligned reduction of N raw products + an optional partial sum.
  *
  * Stages (S1 max-exp → S2 align → S3 sum → S4 round) with optional pipeline
  * cuts after S1 and S3 gated by `latency`.
  *
  * @param config   driving format / anchorHeadroom / accFormat config
  * @param N        number of products to reduce
  * @param latency  0/1/2 — pipeline depth, mirrors MxFpMul
  */
class MxAnchorAccTree(
    val config:  MxConfig,
    val N:       Int,
    val latency: Int = 0
) extends Module {
  require(N >= 1, s"MxAnchorAccTree: N must be >= 1 (got $N)")
  require(latency >= 0 && latency <= 2,
    s"MxAnchorAccTree: latency must be 0/1/2 (got $latency)")

  private val productFmt   = config.productFormat
  private val accFmt       = config.accFormat
  private val productBias  = productFmt.bias
  private val sigW         = accFmt.sig
  private val laneExpWidth = productFmt.exp + 1

  private val sumBits        = log2Ceil(N + 1)
  private val effAnchorHead  = math.max(config.anchorHeadroom, sumBits + 1)
  val intW: Int = sigW + effAnchorHead + sumBits + 1
  val expW: Int = math.max(laneExpWidth, accFmt.exp + 1) + 4
  private val sentinelVal: BigInt = -(BigInt(1) << (expW - 2))

  val io = IO(new Bundle {
    val peMag      = Input(Vec(N, UInt(sigW.W)))
    val peExp      = Input(Vec(N, UInt(laneExpWidth.W)))
    val peSign     = Input(Vec(N, Bool()))
    val peIsZero   = Input(Vec(N, Bool()))
    val peIsNaN    = Input(Bool())
    val rec_c      = Input(UInt(accFmt.recoded.W))
    val accumulate = Input(Bool())
    val out        = Output(UInt(accFmt.recoded.W))
  })

  private def cut[T <: Data](x: T, after: Int): T =
    if (latency >= after) RegNext(x) else x

  // peMag's value mapping is `mag * 2^(unb - sigW + 2)` (the +2 falls out of
  // MxExp.AddBit). cMag's is the standard `mag * 2^(unb - sigW + 1)`; shift
  // its effective unbiased down by 1 so both share one alignment formula.
  private val rawC       = rawFloatFromRecFN(accFmt.exp, accFmt.sig, io.rec_c)
  private val accBiasOff = (BigInt(1) << accFmt.exp).S((accFmt.exp + 2).W)
  private val cUnbiased  = (rawC.sExp -& accBiasOff -& 1.S((accFmt.exp + 2).W)).pad(expW).asSInt
  private val cSign      = rawC.sign
  private val cIsZero    = rawC.isZero || !io.accumulate
  private val cMag       = rawC.sig(sigW - 1, 0)

  private def unbiasedFor(j: Int): SInt = {
    val unb = (io.peExp(j).zext -& productBias.S(expW.W)).pad(expW).asSInt
    Mux(io.peIsZero(j), sentinelVal.S(expW.W), unb)
  }

  private def treeReduce[T](xs: Seq[T])(op: (T, T) => T): T = {
    require(xs.nonEmpty)
    if (xs.length == 1) xs.head
    else {
      val next = xs.grouped(2).map {
        case Seq(a, b) => op(a, b)
        case Seq(a)    => a
      }.toSeq
      treeReduce(next)(op)
    }
  }

  // S1: max-exp anchor.
  private val unbiasedAll: Seq[SInt] =
    (0 until N).map(unbiasedFor) :+ Mux(cIsZero, sentinelVal.S(expW.W), cUnbiased)
  private val maxExp_s0 = treeReduce(unbiasedAll) { (a: SInt, b: SInt) => Mux(a > b, a, b) }
  private val anchor_s0 = (maxExp_s0 +& effAnchorHead.S(expW.W)).pad(expW).asSInt

  // Pipeline cut #1.
  private val anchor_s1    = cut(anchor_s0, 1)
  private val peMag_s1     = VecInit(io.peMag.map(cut(_, 1)))
  private val peExp_s1     = VecInit(io.peExp.map(cut(_, 1)))
  private val peSign_s1    = VecInit(io.peSign.map(cut(_, 1)))
  private val peIsZero_s1  = VecInit(io.peIsZero.map(cut(_, 1)))
  private val peIsNaN_s1   = cut(io.peIsNaN, 1)
  private val cMag_s1      = cut(cMag, 1)
  private val cUnb_s1      = cut(cUnbiased, 1)
  private val cSign_s1     = cut(cSign, 1)
  private val cIsZero_s1   = cut(cIsZero, 1)

  // S2: align each magnitude into a signed intW-bit integer at the anchor.
  private def alignToInt(
      mag: UInt, magW: Int, unb: SInt, sign: Bool, isZero: Bool, anchor: SInt
  ): SInt = {
    val leftShift = (unb -& anchor +& (intW - magW).S(expW.W)).pad(expW).asSInt
    val shiftMag  = leftShift.abs.asUInt
    val shClipW   = log2Ceil(intW + 1)
    val shClip    = Mux(shiftMag > intW.U, intW.U, shiftMag(shClipW - 1, 0))
    val padded    = mag.pad(intW)
    val left      = (padded << shClip)(intW - 1, 0)
    val right     = (padded >> shClip)
    val shifted   = Mux(leftShift >= 0.S, left, right.pad(intW))
    val sgn       = Mux(sign, -shifted.asSInt, shifted.asSInt)
    Mux(isZero, 0.S(intW.W), sgn)
  }

  private val prodInt_s2 = (0 until N).map { j =>
    val unbU = (peExp_s1(j).zext -& productBias.S(expW.W)).pad(expW).asSInt
    alignToInt(peMag_s1(j), sigW, unbU, peSign_s1(j), peIsZero_s1(j), anchor_s1)
  }
  private val cInt_s2 =
    alignToInt(cMag_s1, sigW, cUnb_s1, cSign_s1, cIsZero_s1, anchor_s1)

  // S3: signed tree reduce.
  private val totalWide   = treeReduce(prodInt_s2 :+ cInt_s2) { (a: SInt, b: SInt) => a +& b }
  private val totalInt_s2 = totalWide(intW - 1, 0).asSInt

  // Pipeline cut #2.
  private val totalInt_s3 = cut(totalInt_s2, 2)
  private val anchor_s3   = cut(anchor_s1, 2)
  private val peIsNaN_s3  = cut(peIsNaN_s1, 2)

  // S4: renormalize and round to recoded accFormat.
  private val signFinal = totalInt_s3(intW - 1)
  private val absInt    = Mux(signFinal, (-totalInt_s3).asUInt, totalInt_s3.asUInt)(intW - 1, 0)
  private val isZeroFin = absInt === 0.U
  private val lz        = countLeadingZeros(absInt)
  private val unbFinal  = (anchor_s3 +& 1.S(expW.W) -& lz.zext).pad(expW).asSInt
  private val normInt   = (absInt << lz)(intW - 1, 0)

  private val sExpW   = expW + 2
  private val sExpFin = (unbFinal +& (BigInt(1) << expW).S(sExpW.W)).pad(sExpW).asSInt
  private val raw     = Wire(new RawFloat(expW, intW))
  raw.sign   := signFinal
  raw.isNaN  := peIsNaN_s3
  raw.isInf  := false.B
  raw.isZero := isZeroFin
  raw.sExp   := sExpFin
  raw.sig    := 0.U(1.W) ## normInt

  private val cvt = Module(new RoundAnyRawFNToRecFN(expW, intW, accFmt.exp, accFmt.sig, 0))
  cvt.io.in             := raw
  cvt.io.invalidExc     := false.B
  cvt.io.infiniteExc    := false.B
  cvt.io.roundingMode   := round_near_even
  cvt.io.detectTininess := tininess_afterRounding

  // NaN → 0; infinity → ±max-normal so the output stays within accFormat.
  private val outRaw       = cvt.io.out
  private val outIeee      = fNFromRecFN(accFmt.exp, accFmt.sig, outRaw)
  private val outExpField  = outIeee(accFmt.ieee - 2, accFmt.sig - 1)
  private val outFracField = outIeee(accFmt.sig - 2, 0)
  private val isInfOut     = outExpField.andR && (outFracField === 0.U)
  private val isNaNOut     = outExpField.andR && (outFracField =/= 0.U)
  private val maxNormalIeee = Cat(
    signFinal,
    ((BigInt(1) << accFmt.exp) - 2).U(accFmt.exp.W),
    Fill(accFmt.sig - 1, 1.U(1.W))
  )
  private val sanitizedIeee = Mux(isNaNOut || peIsNaN_s3, 0.U(accFmt.ieee.W),
                                  Mux(isInfOut,           maxNormalIeee, outIeee))
  io.out := recFNFromFN(accFmt.exp, accFmt.sig, sanitizedIeee)
}

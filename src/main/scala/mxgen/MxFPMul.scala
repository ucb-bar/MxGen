package mxgen

import chisel3._
import chisel3.util._
import mxgen.hardfloat._

class MxFpMul(val config: MxConfig, lut: Boolean, val latency: Int = 0) extends Module {
  require(latency >= 0 && latency <= 2,
    s"MxFpMul: latency must be 0, 1, or 2 (got $latency)")
  println("Creating MxFpMul with product precision: " + config.productFormat + " and acc precision: " + config.accFormat + s" (latency=$latency)")
  println(config.describe)

  val cType        = config.accFormat
  val productFmt   = config.productFormat
  val outBias      = productFmt.bias
  val laneExpWidth = productFmt.exp + 1

  val io = IO(new Bundle {
    val in_activation = Input(UInt(config.inActBusWidth.W))
    val type_a = Input(new MxTypeBundle())
    val in_weights = Input(UInt(config.inWeiBusWidth.W))
    val type_w = Input(new MxTypeBundle())
    val mode = Input(new mxMode())
    val enable = Input(Bool())
    val rec_c = Input(UInt((config.numActiveOutputLanes * config.accFormat.recoded).W))
    val out = Output(UInt((config.numActiveOutputLanes * config.accFormat.recoded).W))
  })

  // Mul side lives in MxFpMulCore (shared with MxDotProduct); add side stays
  // here, either per-lane MxPEAddRecFN or fpnew BF16.
  val core = Module(new MxFpMulCore(config, lut))
  core.io.in_activation := io.in_activation
  core.io.type_a        := io.type_a
  core.io.in_weights    := io.in_weights
  core.io.type_w        := io.type_w
  core.io.mode          := io.mode
  core.io.enable        := io.enable

  val peMagW  = core.io.peMag
  val peExpW  = core.io.peExp
  val peSignW = core.io.peSign
  val peZeroW = core.io.peIsZero
  val peIsNaN = core.io.peIsNaN

  // latency=2: one reg before the adder + one reg inside MxPEAddRecFN.
  // fpnew maps `latency` directly to NumPipeRegs.
  val addLatency  = if (latency >= 1) 1 else 0
  val preAddRegs  = if (latency >= 2) 1 else 0
  if (config.useFpnewAdder) {
    require(config.accFormat.exp == 8 && config.accFormat.sig == 8,
      s"useFpnewAdder requires accFormat=BF16(8,8); got ${config.accFormat}")
  }
  val addUnits: Option[Seq[hardfloatHelper.MxPEAddRecFN]] =
    if (config.useFpnewAdder) None
    else Some(Seq.fill(config.numActiveOutputLanes)(
      Module(new hardfloatHelper.MxPEAddRecFN(cType.exp, cType.sig, laneExpWidth, outBias, addLatency))))
  val outputs = Wire(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  val recIn_c = io.rec_c.asTypeOf(Vec(config.numActiveOutputLanes, UInt(config.accFormat.recoded.W)))

  for (i <- 0 until config.numActiveOutputLanes) {
    if (!config.useFpnewAdder) {
      def pipe[T <: chisel3.Data](sig: T, n: Int): T =
        (0 until n).foldLeft(sig)((s, _) => RegNext(s))
      val unit = addUnits.get(i)
      unit.io.roundingMode   := hardfloat.consts.round_near_even
      unit.io.detectTininess := hardfloat.consts.tininess_afterRounding
      unit.io.peMag    := pipe(peMagW(i),  preAddRegs)
      unit.io.peExp    := pipe(peExpW(i),  preAddRegs)
      unit.io.peSign   := pipe(peSignW(i), preAddRegs)
      unit.io.peIsZero := pipe(peZeroW(i), preAddRegs)
      unit.io.peIsNaN  := pipe(peIsNaN,    preAddRegs)
      unit.io.c        := pipe(recIn_c(i), preAddRegs)

      outputs(i) := unit.io.out
    }
  }

  // fpnew/cvfpu BF16 adder path: round raw product to BF16, then add.
  if (config.useFpnewAdder) {
    val numLanes     = config.numActiveOutputLanes
    val productBias  = config.productFormat.bias
    val peMagWidth   = cType.sig
    val peExpWidth   = laneExpWidth
    val sExpW        = peExpWidth + 2
    val recodedOff   = (BigInt(1) << peExpWidth) - BigInt(productBias)

    def peProductToBf16Ieee(
      peMag: UInt, peExp: UInt, peSign: Bool, peIsZero: Bool, peIsNaN: Bool
    ): UInt = {
      val isZero   = peIsZero || (peMag === 0.U)
      val normDist = countLeadingZeros(peMag)
      val normMag  = (peMag << normDist)(peMagWidth - 1, 0)
      val sExpUInt = peExp +& recodedOff.U(sExpW.W) - normDist
      val sExpSInt = sExpUInt(sExpW - 1, 0).asSInt
      val sig = 0.U(1.W) ## (!isZero) ## normMag(peMagWidth - 2, 0)

      val raw = Wire(new RawFloat(peExpWidth, peMagWidth))
      raw.isNaN  := peIsNaN
      raw.isInf  := false.B
      raw.isZero := isZero
      raw.sign   := peSign
      raw.sExp   := sExpSInt
      raw.sig    := sig

      val cvt = Module(new RoundAnyRawFNToRecFN(peExpWidth, peMagWidth, 8, 8, 0))
      cvt.io.in             := raw
      cvt.io.invalidExc     := false.B
      cvt.io.infiniteExc    := false.B
      cvt.io.roundingMode   := hardfloat.consts.round_near_even
      cvt.io.detectTininess := hardfloat.consts.tininess_afterRounding
      fNFromRecFN(8, 8, cvt.io.out)
    }

    val laneProductIeee = (0 until numLanes).map { i =>
      peProductToBf16Ieee(peMagW(i), peExpW(i), peSignW(i), peZeroW(i), peIsNaN)
    }
    val laneCIeee = (0 until numLanes).map { i =>
      fNFromRecFN(8, 8, recIn_c(i))
    }
    val adder = Module(new mxgen.cvfpu.MxFpnewBf16Add(numLanes = numLanes, numPipeRegs = latency))
    adder.io.clock := clock
    adder.io.reset := reset
    adder.io.a_i   := Cat(laneProductIeee.reverse)
    adder.io.b_i   := Cat(laneCIeee.reverse)

    val outVec = adder.io.out_o.asTypeOf(Vec(numLanes, UInt(16.W)))
    for (i <- 0 until numLanes) {
      outputs(i) := recFNFromFN(8, 8, outVec(i))
    }
  }

  io.out := outputs.asUInt
}

object MxPEOutToRaw {
  def apply(expWidth: Int, sigWidth: Int, sign: UInt, exp: UInt, sig: UInt, inputNaN: Bool, inputZero: Bool): RawFloat = {
    val expIn = exp(expWidth, 0)
    val fractIn = sig(sigWidth-2, 0)

    val isZeroExpIn = (expIn === 0.U)
    val isZeroFractIn = (fractIn === 0.U)

    val normDist = countLeadingZeros(fractIn)
    val subnormFract = if (sigWidth > 2) {
      (fractIn << normDist) (sigWidth - 3, 0) << 1
    } else {
      0.U
    }
    val adjustedExp =
      Mux(isZeroExpIn,
        normDist ^ ((BigInt(1) << (expWidth + 1)) - 1).U,
        expIn
      ) + ((BigInt(1) << (expWidth - 1)).U
        | Mux(isZeroExpIn, 2.U, 1.U))

    val isZero = isZeroExpIn && isZeroFractIn
    val isSpecial = adjustedExp(expWidth, expWidth - 1) === 3.U

    val satExp  = (BigInt(3) << (expWidth - 1)).U((expWidth + 1).W)
    val satFrac = ((BigInt(1) << (sigWidth - 1)) - 2).U((sigWidth - 1).W)
    val saturate     = isSpecial && !inputNaN
    val effectiveZero = (isZero || inputZero) && !inputNaN

    val out = Wire(new RawFloat(expWidth, sigWidth))
    out.isNaN  := inputNaN
    out.isInf  := false.B
    out.isZero := effectiveZero
    out.sign   := sign
    out.sExp   := Mux(inputNaN || saturate, satExp, adjustedExp(expWidth, 0)).zext
    out.sig    := 0.U(1.W) ## !effectiveZero ## Mux(inputNaN || saturate, satFrac, Mux(isZeroExpIn, subnormFract, fractIn))
    out
  }
}

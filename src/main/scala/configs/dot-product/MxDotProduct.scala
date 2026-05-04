package mxgen

import chisel3._
import chisel3.util._

/** Dot-product wrapper: numCores × MxFpMulCore feeding one MxAnchorAccTree.
  *
  * Lane geometry is runtime-derived: fp4/fp6 modes give 4 products/core
  * (numCores*4 total), fp8 gives 1 (numCores total). Lanes inactive for the
  * current mode are gated to peIsZero so the tree treats them as 0.
  *
  * @param config       MxConfig (productFormat, accFormat, anchorHeadroom, …)
  * @param lut          forwarded to each MxFpMulCore's MxPE
  * @param numCores     1 or 4 — number of MxFpMulCore instances. numCores=1
  *                     requires fixedNumOutputs==4 (fp4/fp6-only configs).
  * @param latency      0/1/2 total dot-product latency, mirrors MxFpMul.
  * @param coreLatency  register stages between cores and tree input;
  *                     must be ≤ latency. The tree absorbs the remaining
  *                     (latency - coreLatency) stages.
  */
class MxDotProduct(
    val config:      MxConfig,
    val lut:         Boolean,
    val numCores:    Int = 4,
    val latency:     Int = 0,
    val coreLatency: Int = 0
) extends Module {
  require(latency >= 0 && latency <= 2,
    s"MxDotProduct: latency must be 0/1/2 (got $latency)")
  require(coreLatency >= 0 && coreLatency <= latency,
    s"MxDotProduct: coreLatency ($coreLatency) must be in [0, latency=$latency]")
  require(numCores == 1 || numCores == 4,
    s"MxDotProduct: numCores must be 1 or 4 (got $numCores)")
  if (numCores == 1) {
    require(config.fixedNumOutputs.contains(4),
      "MxDotProduct: numCores=1 requires a fp4/fp6-only config (fixedNumOutputs == 4)")
  }

  println(s"Creating MxDotProduct: numCores=$numCores, latency=$latency, " +
          s"coreLatency=$coreLatency, accFormat=${config.accFormat}, " +
          s"productFormat=${config.productFormat}")

  val accRec       = config.accFormat.recoded
  val numActive    = config.numActiveOutputLanes
  val totalLanes   = numCores * numActive
  val laneExpWidth = config.productFormat.exp + 1
  val sigW         = config.accFormat.sig

  val io = IO(new Bundle {
    val in_activation = Input(UInt((numCores * config.inActBusWidth).W))
    val type_a        = Input(new MxTypeBundle())
    val in_weights    = Input(UInt((numCores * config.inWeiBusWidth).W))
    val type_w        = Input(new MxTypeBundle())
    val mode          = Input(new mxMode())
    val enable        = Input(Bool())
    val rec_c         = Input(UInt(accRec.W))
    val accumulate    = Input(Bool())
    val out           = Output(UInt(accRec.W))
  })

  // Lane-validity mask gated by mode.numOutputs (folds away when static).
  val laneValid: Seq[Bool] = config.fixedNumOutputs match {
    case Some(_) => Seq.fill(numActive)(true.B)
    case None    => (0 until numActive).map(i => i.U < io.mode.numOutputs)
  }

  val actSlice = io.in_activation.asTypeOf(Vec(numCores, UInt(config.inActBusWidth.W)))
  val weiSlice = io.in_weights.asTypeOf(Vec(numCores, UInt(config.inWeiBusWidth.W)))

  val peMag      = Wire(Vec(totalLanes, UInt(sigW.W)))
  val peExp      = Wire(Vec(totalLanes, UInt(laneExpWidth.W)))
  val peSign     = Wire(Vec(totalLanes, Bool()))
  val peIsZero   = Wire(Vec(totalLanes, Bool()))
  val peIsNaNVec = Wire(Vec(numCores, Bool()))

  for (c <- 0 until numCores) {
    val core = Module(new MxFpMulCore(config, lut))
    core.io.in_activation := actSlice(c)
    core.io.in_weights    := weiSlice(c)
    core.io.type_a        := io.type_a
    core.io.type_w        := io.type_w
    core.io.mode          := io.mode
    core.io.enable        := io.enable

    for (i <- 0 until numActive) {
      val g = c * numActive + i
      peMag(g)    := core.io.peMag(i)
      peExp(g)    := core.io.peExp(i)
      peSign(g)   := core.io.peSign(i)
      peIsZero(g) := core.io.peIsZero(i) || !laneValid(i)
    }
    peIsNaNVec(c) := core.io.peIsNaN
  }
  val peIsNaN = peIsNaNVec.asUInt.orR

  def pipe[T <: Data](x: T, n: Int): T =
    (0 until n).foldLeft(x)((s, _) => RegNext(s))

  val tree = Module(new MxAnchorAccTree(config, totalLanes, latency = latency - coreLatency))
  for (i <- 0 until totalLanes) {
    tree.io.peMag(i)    := pipe(peMag(i),    coreLatency)
    tree.io.peExp(i)    := pipe(peExp(i),    coreLatency)
    tree.io.peSign(i)   := pipe(peSign(i),   coreLatency)
    tree.io.peIsZero(i) := pipe(peIsZero(i), coreLatency)
  }
  tree.io.peIsNaN    := pipe(peIsNaN,       coreLatency)
  tree.io.rec_c      := pipe(io.rec_c,      coreLatency)
  tree.io.accumulate := pipe(io.accumulate, coreLatency)

  io.out := tree.io.out
}

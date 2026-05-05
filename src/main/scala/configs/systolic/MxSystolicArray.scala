package mxgen

import chisel3._
import chisel3.util._

/** NxN weight-stationary systolic array of MxFpMul PEs (the "flex" PE — each
  * cell is configured by its own MxConfig).
  */
class MxSystolicArray(
    val configs: Seq[MxConfig],
    val lut:     Boolean = false,
    val latency: Int     = 0
) extends Module {
  require(configs.nonEmpty, "MxSystolicArray: configs must be non-empty")
  val n = configs.length

  private val head = configs.head
  private val actBusW  = head.inActBusWidth
  private val weiBusW  = head.inWeiBusWidth
  private val accFmt   = head.accFormat
  private val numLanes = head.numActiveOutputLanes
  configs.foreach { c =>
    require(c.inActBusWidth        == actBusW,  "MxSystolicArray: inActBusWidth must match across all rows")
    require(c.inWeiBusWidth        == weiBusW,  "MxSystolicArray: inWeiBusWidth must match across all rows")
    require(c.accFormat            == accFmt,   "MxSystolicArray: accFormat must match across all rows")
    require(c.numActiveOutputLanes == numLanes, "MxSystolicArray: numActiveOutputLanes must match across all rows")
  }

  private val psumW = numLanes * accFmt.recoded

  val io = IO(new Bundle {
    val in_activation = Input(Vec(n, UInt(actBusW.W)))
    val type_a        = Input(Vec(n, new MxTypeBundle()))
    val in_weights    = Input(Vec(n, UInt(weiBusW.W)))
    val type_w        = Input(Vec(n, new MxTypeBundle()))
    val mode          = Input(Vec(n, Vec(n, new mxMode())))
    val enable        = Input(Bool())
    val load_weights  = Input(Bool())
    val psum_in       = Input(Vec(n, UInt(psumW.W)))
    val out           = Output(Vec(n, UInt(psumW.W)))
  })

  val pes: Seq[Seq[MxFpMul]] = (0 until n).map { r =>
    (0 until n).map { c =>
      val pe = Module(new MxFpMul(configs(r), lut, latency))
      pe.io.enable := io.enable
      pe.io.mode   := io.mode(r)(c)
      pe
    }
  }

  for (r <- 0 until n) {
    (0 until n).foldLeft((io.in_activation(r), io.type_a(r))) {
      case ((act, typ), c) =>
        pes(r)(c).io.in_activation := act
        pes(r)(c).io.type_a        := typ
        (RegNext(act), RegNext(typ))
    }
  }

  for (c <- 0 until n) {
    (0 until n).foldLeft((io.in_weights(c), io.type_w(c))) {
      case ((wei, typ), r) =>
        val wReg = RegEnable(wei, 0.U(weiBusW.W), io.load_weights)
        val tReg = RegEnable(typ, 0.U.asTypeOf(new MxTypeBundle()), io.load_weights)
        pes(r)(c).io.in_weights := wReg
        pes(r)(c).io.type_w     := tReg
        (wReg, tReg)
    }
  }

  for (c <- 0 until n) {
    val tail = (0 until n).foldLeft(io.psum_in(c)) {
      case (psum, r) =>
        pes(r)(c).io.rec_c := psum
        RegNext(pes(r)(c).io.out)
    }
    io.out(c) := tail
  }
}

object MxSystolicArray {
  def uniform(n: Int, config: MxConfig, lut: Boolean = false, latency: Int = 0): MxSystolicArray =
    Module(new MxSystolicArray(Seq.fill(n)(config), lut, latency))
}

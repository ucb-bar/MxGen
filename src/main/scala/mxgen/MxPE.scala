package mxgen

import chisel3._
import chisel3.util._

class MxPE(config: MxConfig, lut: Boolean) extends Module {
  val io = IO(new Bundle {
    val modeDecoded = Input(new mxMode())
    val in_a = Input(UInt(config.inPE_act_totalWidth.W))
    val in_w = Input(UInt(config.inPE_wei_totalWidth.W))
    val mask_a = Input(UInt(2.W))
    val mask_w = Input(UInt(2.W))
    val enable = Input(Bool())
    val output = Output(UInt(config.outPE_width.W))
  })

  val flexMults = Seq.fill(2,2){ Module(new MACU(lut)) }
  val outFM = Wire(Vec(4, UInt(config.multOutWidth.W)))

  for (i <- 0 until 2) {
    for (j <- 0 until 2) {
      val fm = flexMults(i)(j)
      val a = Wire(UInt(config.actflexMulInWidth.W))
      val a_en = Wire(Bool())
      val w_en = Wire(Bool())

      // --- Activation input routing (gated by fixedActInputs) ---
      if (config.inPE_act_width == 4) {
        config.fixedActInputs match {
          case Some(1) =>
            a := io.in_a(2*(i+1) - 1, 2*i)
            a_en := io.mask_a(0)
          case Some(_) =>
            a := io.in_a(config.actflexMulInWidth*(i+1) - 1, config.actflexMulInWidth*i)
            a_en := io.mask_a(i)
          case None =>
            when (io.modeDecoded.actInputs === 1.U) {
              a := io.in_a(2*(i+1) - 1, 2*i)
              a_en := io.mask_a(0)
            } .otherwise {
              a := io.in_a(config.actflexMulInWidth*(i+1) - 1, config.actflexMulInWidth*i)
              a_en := io.mask_a(i)
            }
        }
      } else {
        a := io.in_a(config.actflexMulInWidth*(i+1) - 1, config.actflexMulInWidth*i)
        a_en := io.mask_a(i)
      }

      // --- Weight input routing (gated by fixedWeiInputs) ---
      val w = Wire(UInt(config.weiflexMulInWidth.W))

      if (config.numWeiInputs < 3) {
        if (config.inPE_wei_width == 4) {
          config.fixedWeiInputs match {
            case Some(1) =>
              w := io.in_w(2*(j+1) - 1, 2*j).pad(config.weiflexMulInWidth)
              w_en := io.mask_w(0)
            case Some(_) =>
              w := io.in_w(config.weiflexMulInWidth*(j+1) - 1, config.weiflexMulInWidth*j)
              w_en := io.mask_w(j)
            case None =>
              when (io.modeDecoded.weiInputs === 1.U) {
                w := io.in_w(2*(j+1) - 1, 2*j).pad(config.weiflexMulInWidth)
                w_en := io.mask_w(0)
              } .otherwise {
                w := io.in_w(config.weiflexMulInWidth*(j+1) - 1, config.weiflexMulInWidth*j)
                w_en := io.mask_w(j)
              }
          }
        } else {
          w := io.in_w(config.weiflexMulInWidth*(j+1) - 1, config.weiflexMulInWidth*j)
          w_en := io.mask_w(j)
        }
      } else {
        config.fixedWeiInputs match {
          case Some(1) =>
            w := io.in_w(2*(j+1) - 1, 2*j).pad(config.weiflexMulInWidth)
            w_en := io.mask_w(0)
          case Some(n) if n < 3 =>
            w := io.in_w(config.weiflexMulInWidth*(j*2+1) - 1, config.weiflexMulInWidth*j*2)
            w_en := io.mask_w(j)
          case Some(_) =>
            w := io.in_w(config.weiflexMulInWidth*(i*2+j + 1) - 1, config.weiflexMulInWidth*((i*2+j)))
            w_en := io.mask_w(j)
          case None =>
            when (io.modeDecoded.weiInputs === 1.U) {
              w := io.in_w(2*(j+1) - 1, 2*j).pad(config.weiflexMulInWidth)
              w_en := io.mask_w(0)
            } .elsewhen (io.modeDecoded.weiInputs < 3.U) {
              w := io.in_w(config.weiflexMulInWidth*(j*2+1) - 1, config.weiflexMulInWidth*j*2)
              w_en := io.mask_w(j)
            }.otherwise {
              w := io.in_w(config.weiflexMulInWidth*(i*2+j + 1) - 1, config.weiflexMulInWidth*((i*2+j)))
              w_en := io.mask_w(j)
            }
        }
      }

      fm.io.w := a(config.actflexMulInWidth - 1, 0)
      fm.io.act := w(config.weiflexMulInWidth - 1, 0)
      fm.io.enable := io.enable && a_en && w_en
      fm.io.w_mode := (config.weiflexMulInWidth == 3 || config.actflexMulInWidth == 3).B
      fm.io.act_mode := (config.actflexMulInWidth == 3 || config.weiflexMulInWidth == 3).B

      if (config.multOutWidth == 6) {
        outFM(i*2 + j) := fm.io.output
      } else if (config.multOutWidth == 5) {
        outFM(i*2 + j) := fm.io.output(4,0)
      } else {
        outFM(i*2 + j) := fm.io.output(3,0)
      }
    }
  }

  // 4-output mode: each MACU result lands on its own lane, zero-padded to
  // `laneOutputWidths` if set, otherwise packed at `multOutWidth`.
  val out4Packed: UInt = if (config.laneOutputWidths.isDefined) {
    VecInit((0 until 4).map(i => outFM(i).pad(config.laneWidths(i)))).asUInt
  } else {
    outFM.asUInt
  }

  // --- Output routing (gated by needsOut1/needsOut2/needsOut4) ---
  if (config.inPE_act_width == 4 || config.inPE_wei_width == 4) {
    // Shift-and-add paths only needed for 1- or 2-output modes.
    if (config.needsOut1 || config.needsOut2) {
      val outShift0 = outFM(3) << io.modeDecoded.shift(0)(0)
      val outShift1 = outFM(2) << io.modeDecoded.shift(0)(1)
      val outShift2 = outFM(1) << io.modeDecoded.shift(1)(0)
      val outShift3 = outFM(0) << io.modeDecoded.shift(1)(1)

      val out1 = if (config.needsOut1) {
        Some((outShift0 + outShift1 + outShift2 + outShift3).pad(config.outPE_width))
      } else None

      val out2 = if (config.needsOut2) {
        val sum2 = Mux(io.modeDecoded.actWidth === 4.U, outShift0 + outShift2, outShift0 + outShift1)
        val sum3 = Mux(io.modeDecoded.actWidth === 4.U, outShift1 + outShift3, outShift2 + outShift3)
        val v = Wire(Vec(2, UInt((config.outPE_width / 2).W)))
        v(0) := sum3.pad(config.outPE_width / 2)
        v(1) := sum2.pad(config.outPE_width / 2)
        Some(v.asUInt)
      } else None

      config.fixedNumOutputs match {
        case Some(1) => io.output := out1.get
        case Some(2) => io.output := out2.get
        case None =>
          (config.needsOut1, config.needsOut2, config.needsOut4) match {
            case (true, true, true) =>
              io.output := Mux(io.modeDecoded.numOutputs === 1.U, out1.get,
                Mux(io.modeDecoded.numOutputs === 2.U, out2.get, out4Packed))
            case (true, false, true) =>
              io.output := Mux(io.modeDecoded.numOutputs === 1.U, out1.get, out4Packed)
            case (false, true, true) =>
              io.output := Mux(io.modeDecoded.numOutputs === 2.U, out2.get, out4Packed)
            case (true, true, false) =>
              io.output := Mux(io.modeDecoded.numOutputs === 1.U, out1.get, out2.get)
            case _ => io.output := out4Packed
          }
        case _ => io.output := out4Packed
      }
    } else {
      io.output := out4Packed
    }
  } else {
    io.output := out4Packed
  }
}

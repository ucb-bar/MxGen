package mxgen 

import chisel3._
import chisel3.util._

class MxPE(config: MxConfig, lut: Boolean) extends Module {
  require(config.inPE_wei_totalWidth == config.inPE_act_totalWidth, "inPE_wei_totalWidth must equal inPE_act_totalWidth for this MxPE implementation")
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

  // printf(p"enable: ${io.enable}, mask_a: ${Binary(io.mask_a)}, mask_w: ${Binary(io.mask_w)}\n")  // --- IGNORE ---
  // printf(p"in_a: ${Binary(io.in_a)}, in_w: ${Binary(io.in_w)}\n")  // --- IGNORE ---

  for (i <- 0 until 2) {
    for (j <- 0 until 2) {
      val fm = flexMults(i)(j)
      val a = Wire(UInt(config.actflexMulInWidth.W))
      val a_en = Wire(Bool())
      val w_en = Wire(Bool())

      if (config.inPE_act_width == 4) {
          when (io.modeDecoded.actInputs === 1.U) {
            a := io.in_a(2*(i+1) - 1, 2*i)
            a_en := io.mask_a(0)
          } .otherwise {
            a := io.in_a(config.actflexMulInWidth*(i+1) - 1, config.actflexMulInWidth*i)
            a_en := io.mask_a(i)
          }
      } else {
        a := io.in_a(config.actflexMulInWidth*(i+1) - 1, config.actflexMulInWidth*i)
        a_en := io.mask_a(i)
      }

      val w = Wire(UInt(config.weiflexMulInWidth.W))

      if (config.numWeiInputs < 3) {
        if (config.inPE_wei_width == 4) {
          when (io.modeDecoded.weiInputs === 1.U) {
            w := io.in_w(2*(j+1) - 1, 2*j).pad(config.weiflexMulInWidth)
            w_en := io.mask_w(0)
          } .otherwise {
            w := io.in_w(config.weiflexMulInWidth*(j+1) - 1, config.weiflexMulInWidth*j)
            w_en := io.mask_w(j)
          }
        } else {
          w := io.in_w(config.weiflexMulInWidth*(j+1) - 1, config.weiflexMulInWidth*j)
          w_en := io.mask_w(j)
        }
      } else {
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

      // printf(p"a: ${Binary(a)} w: ${Binary(w)} \n")

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

  // In the 4-output case each of the 4 MACU results lands on its own output
  // lane. By default every lane uses the same `multOutWidth` bits (packed
  // back-to-back), but when `MxConfig.laneOutputWidths` is set each MACU
  // output is zero-padded to its per-lane width before concatenation.
  val out4Packed: UInt = if (config.laneOutputWidths.isDefined) {
    VecInit((0 until 4).map(i => outFM(i).pad(config.laneWidths(i)))).asUInt
  } else {
    outFM.asUInt
  }

  if (config.inPE_act_width == 4 || config.inPE_wei_width == 4) {
    val outShift0 = outFM(3) << io.modeDecoded.shift(0)(0)
    val outShift1 = outFM(2) << io.modeDecoded.shift(0)(1)
    val outShift2 = outFM(1) << io.modeDecoded.shift(1)(0)
    val outShift3 = outFM(0) << io.modeDecoded.shift(1)(1)

    val sum1 = outShift0 + outShift1 + outShift2 + outShift3
    val sum2 = Mux(io.modeDecoded.actWidth === 4.U, outShift0 + outShift2, outShift0 + outShift1)
    val sum3 = Mux(io.modeDecoded.actWidth === 4.U, outShift1 + outShift3, outShift2 + outShift3)

    val out1 = sum1.pad(config.outPE_width)
    val out2 = Wire(Vec(2, UInt((config.outPE_width / 2).W)))
    out2(0) := sum3.pad((config.outPE_width / 2))
    out2(1) := sum2.pad((config.outPE_width / 2))

    io.output := Mux(io.modeDecoded.numOutputs === 1.U, out1.asUInt,
                   Mux(io.modeDecoded.numOutputs === 2.U, out2.asUInt,
                     out4Packed))

  } else {
    io.output := out4Packed
  }
}
package mxgen

import chisel3._
import chisel3.util._

/** Which of MxFpMul's product cones (4-, 2-, 1-output) its output select takes for a decoded mode, mirroring the
  * when / elsewhen chain in MxFpMul exactly (including the fall-through for modes a config does not support), and
  * which lanes of the packed PE product those cones read. Shared by the PE (multiplier gating) and MxFpMul (cone
  * gating) so both gate precisely what the select discards.
  */
object MxLaneUse {
  def cones(config: MxConfig, nOut: UInt): (Bool, Bool, Bool) = {
    val is1 = nOut === 1.U
    val is2 = nOut === 2.U
    (config.needsOut4, config.needsOut2, config.needsOut1) match {
      case (true, true, true)   => (!is1 && !is2, is2, is1)
      case (true, false, true)  => (!is1, false.B, is1)
      case (true, true, false)  => (!is2, is2, false.B)
      case (false, true, true)  => (false.B, !is1, is1)
      case (true, false, false) => (true.B, false.B, false.B)
      case (false, true, false) => (false.B, true.B, false.B)
      case (false, false, true) => (false.B, false.B, true.B)
      case _                    => (true.B, true.B, true.B)
    }
  }
  /** lane k of the packed product is read by a cone in use: the 4-cone reads every lane, the 2-cone the 7 bits at
    * offsets 0 and outPE_width/2, the 1-cone the single mode's product width from offset 0 */
  def laneUsed(config: MxConfig, k: Int, use4: Bool, use2: Bool, use1: Bool): Bool = {
    val lo = config.laneOffset(k); val hi = lo + config.laneWidth(k)
    def overlaps(a: Int, b: Int): Boolean = lo < b && a < hi
    val half  = config.outPE_width / 2
    val prodW = config.modesSupported.find(_.numOutputs == 1).map(_.outTotalWidth).getOrElse(config.outPE_width)
    val in2 = overlaps(0, 7) || overlaps(half, half + 7)
    val in1 = overlaps(0, prodW)
    use4 || (use2 && in2.B) || (use1 && in1.B)
  }
}

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

  if (config.hasMode9) {
    // ---- Quad (16-MACU) datapath: one LaneMul per output lane. ----
    // E4M3 is a dual format here, so in_a/in_w carry 2 significand slots.
    val slotAW = config.inPE_act_totalWidth / 2
    val slotWW = config.inPE_wei_totalWidth / 2
    val actSlots = io.in_a.asTypeOf(Vec(2, UInt(slotAW.W)))
    val weiSlots = io.in_w.asTypeOf(Vec(2, UInt(slotWW.W)))

    val actIs4 = io.modeDecoded.actWidth === 4.U
    val weiIs4 = io.modeDecoded.weiWidth === 4.U
    val is4x4 = actIs4 && weiIs4
    // Exactly one operand is 4-bit: the {hi,lo}-split multiply, for both the 4-product quad modes (10/11)
    // and the 2-product dual-throughput modes (2/5/6/7, one operand single).
    val is4xN = (actIs4 =/= weiIs4)
    val actM2 = io.modeDecoded.actWidth === 3.U
    val weiM2 = io.modeDecoded.weiWidth === 3.U

    // Map operand slots to lanes. 4-output: lane k = act(k/2) x wei(k%2) -> [a0w0,a0w1,a1w0,a1w1].
    // 2-output: the dual operand's two elements go to lanes 0 and 2 (where out2_toRec reads); a single
    // operand always uses slot 0.
    val two = io.modeDecoded.numOutputs === 2.U
    // Lane gating (config.laneGating): the 1-output cone reads lane 0 only and the 2-output cone lane 0 and the lane
    // at the half-bus offset (MxFpMul.out1_toRec / out2_toRec); the other LaneMuls computed duplicates. Hold their
    // operands at zero and drop their enable so the 16 MACUs behind them do not toggle.
    val (use4, use2, use1) = MxLaneUse.cones(config, io.modeDecoded.numOutputs)
    def laneUsed(k: Int): Bool =
      if (!config.laneGating || k == 0) true.B else MxLaneUse.laneUsed(config, k, use4, use2, use1)
    val laneOuts = (0 until 4).map { k =>
      val lm = Module(new LaneMul(lut))
      val used = laneUsed(k)
      val actIdx = Mux(io.modeDecoded.actInputs === 1.U, 0.U, (k / 2).U)
      val weiIdx = Mux(io.modeDecoded.weiInputs === 1.U, 0.U, Mux(two, (k / 2).U, (k % 2).U))
      lm.io.act       := Mux(used, actSlots(actIdx).pad(4), 0.U)
      lm.io.wei       := Mux(used, weiSlots(weiIdx).pad(4), 0.U)
      lm.io.is4x4     := is4x4
      lm.io.is4xN     := is4xN
      lm.io.wideIsAct := actIs4
      lm.io.actMode   := actM2
      lm.io.weiMode   := weiM2
      lm.io.enable    := io.enable && used
      lm.io.out
    }
    io.output := VecInit((0 until 4).map(k => laneOuts(k).pad(config.laneWidths(k)))).asUInt
  } else {

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
  } // end else (non-mode9 compact PE)
}

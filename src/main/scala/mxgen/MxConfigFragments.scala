package mxgen

import chisel3._
import chisel3.util._

case class MxFloat(expWidth: Int, sigWidth: Int, count: Int, isRecoded: Boolean = false, pad: Boolean = true) extends Bundle {
  val bits = if (pad) {
    UInt((1<<log2Ceil(count * (expWidth + sigWidth + (if (isRecoded) 1 else 0)))).W)
  } else {
    UInt((count * (expWidth + sigWidth + (if (isRecoded) 1 else 0))).W)
  }
  val bias: Int = (1 << (expWidth-1)) - 1
}
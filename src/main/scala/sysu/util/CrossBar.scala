package sysu.util

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import scala.util.Random

import sysu.xilinx._
import sysu.util._

// design : 使用Area实现的轻量化util模块,参数信息往往不是显式给出,而是藏在接入的data中
// @param switch : 其中每个BitVector对应一个输出端口
class CrossBar(input: Vec[Bits], switch: Vec[Bits]) extends ImplicitArea[Vec[Bits]] {

  val bitWidth = input(0).getBitsWidth
  val inputCount = input.length
  val outputCount = switch.length
  require(switch(0).getBitsWidth == inputCount, "switch size must be compatible with input count")

  val output = Vec(Bits(bitWidth bits), outputCount)
  (0 until outputCount).foreach(i => output(i) := MuxOH(switch(i), input))

  // design : priority - lower index first
  override def implicitValue: Vec[Bits] = output
}

object CrossBar {
  def apply(input: Vec[Bits], switch: Vec[Bits]): CrossBar = new CrossBar(input, switch)
}


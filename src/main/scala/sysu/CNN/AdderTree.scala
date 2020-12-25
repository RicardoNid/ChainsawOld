package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.simple.PipelinedMemoryBus

import scala.util.Random

class AdderTree(operands: Seq[UInt], pipelined: Boolean = true) extends ImplicitArea[UInt] {

  //  require(isPow2(numOperand), "number of operands should be power of 2")
  val numOperand = operands.length
  val bitWidth = operands(0).getBitsWidth
  val outWidthGrowth = log2Up(numOperand)
  val result = out UInt ((bitWidth + outWidthGrowth) bits)

  def tree(op: Seq[UInt]): Seq[UInt] = {
    val n = op.length
    if (n == 1) op
    else {
      val half = (n + 1) / 2
      val mid = Range(0, half).map(i => if (n % 2 == 1 && i == half - 1) op(i) else op(i) +^ op(i + half))
      mid.foreach(signal => signal.addAttribute("dont_touch = \"yes\""))
      if (pipelined) tree(mid.map(op => RegNext(op))) else tree(mid)
    }
  }

  result := tree(operands)(0).resized

  override def implicitValue: UInt = this.result
}

object AdderTree {
  def apply(operands: Seq[UInt], pipelined: Boolean = false): AdderTree = new AdderTree(operands, pipelined)
}

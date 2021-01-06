package sysu.util

import spinal.core._
import spinal.lib._

class SignedAdderTree(operands: Seq[SInt], level: Int = 0) extends ImplicitArea[SInt] {

  //  require(isPow2(numOperand), "number of operands should be power of 2")
  val numOperand = operands.length
  val bitWidth = operands(0).getBitsWidth
  val outWidthGrowth = log2Up(numOperand)
  val result = SInt(bitWidth + outWidthGrowth bits)

  def tree(operands: Seq[SInt], depth: Int = outWidthGrowth - 1): Seq[SInt] = {
    val n = operands.length
    if (n > 1) {
      val half = (n + 1) / 2
      val mid = (0 until half).map(i => if (n % 2 == 1 && i == half - 1) operands(i) else operands(i) +^ operands(i + half)).map(_.addAttribute("dont_touch = \"yes\""))
      if (level == 0) tree(mid) else if (depth % level != 0) tree(mid, depth - 1) else tree(mid.map(RegNext(_)), depth - 1)
    }
    else operands
  }

  val latency = LatencyAnalysis(operands(0), result) // 暴露给用户

  override def implicitValue: SInt = tree(operands)(0).resized
}

object SignedAdderTree {
  def apply(operands: Seq[SInt], level: Int = 0): SignedAdderTree = new SignedAdderTree(operands, level)
}

package DSP

import spinal.core._

// TODO: find a type to include both UInt and SInt, but only the two of them
class AdderTree(operands: Seq[SInt], level: Int = 0) extends ImplicitArea[SInt] {

  //  require(isPow2(numOperand), "number of operands should be power of 2")
  val numOperand = operands.length

  // TODO: add another mode
  val bitWidth = operands.map(_.getBitsWidth).max
  val outWidthGrowth = 1

//  val bitWidth = operands(0).getBitsWidth
//  val outWidthGrowth = log2Up(numOperand)

  // TODO: optimize the adderTree for bitWidth considerations
  def tree(operands: Seq[SInt], depth: Int = log2Up(numOperand) - 1): Seq[SInt] = {
    val n = operands.length
    if (n > 1) {
      val half = (n + 1) / 2
      val mid = (0 until half).map(i => if (n % 2 == 1 && i == half - 1) operands(i) else operands(i) +^ operands(i + half)).map(_.addAttribute("dont_touch = \"yes\""))
      if (level == 0) tree(mid) else if (depth % level != 0) tree(mid, depth - 1) else tree(mid.map(RegNext(_)), depth - 1)
    }
    else operands
  }

  override def implicitValue: SInt = tree(operands)(0).resized
}

object AdderTree {

  def apply(operands: Seq[SInt], level: Int = 0): AdderTree = new AdderTree(operands, level)

}

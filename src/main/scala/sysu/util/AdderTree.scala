package sysu.util

import spinal.core._
import sysu.xilinx._


/*
  operands : 加法树输入
  level : 组合逻辑级数
 */

class AdderTree(operands: Seq[UInt], level: Int = 0) extends ImplicitArea[UInt] {

  //  require(isPow2(numOperand), "number of operands should be power of 2")
  val numOperand = operands.length
  val bitWidth = operands(0).getBitsWidth
  val outWidthGrowth = log2Up(numOperand)

  def tree(operands: Seq[UInt], depth: Int = outWidthGrowth - 1): Seq[UInt] = {
    val n = operands.length
    if (n > 1) {
      val half = (n + 1) / 2
      val mid = (0 until half).map(i => if (n % 2 == 1 && i == half - 1) operands(i) else operands(i) +^ operands(i + half)).map(_.addAttribute("dont_touch = \"yes\""))
      if (level == 0) tree(mid) else if (depth % level != 0) tree(mid, depth - 1) else tree(mid.map(RegNext(_)), depth - 1)
    }
    else operands
  }

  override def implicitValue: UInt = tree(operands)(0).resized
}

object AdderTree {

  def apply(operands: Seq[UInt], level: Int = 0): AdderTree = new AdderTree(operands, level)

  def main(args: Array[String]): Unit = {
    val report = VivadoFlow(design = new Component {
      val io = new Bundle {
        val input = in Vec(UInt(8 bits), 19)
        val output = out UInt (13 bits)
      }
      io.output := AdderTree(io.input, 3).resized
    },
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = recommended.vivadoTaskTemp,
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

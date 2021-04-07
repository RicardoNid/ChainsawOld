package DSP

import DSP.ShiftAdderTree.shiftAdderTree
import spinal.core._
import spinal.lib._
import xilinx.VivadoFlow //  for digital signal processing

class ShiftAdderTree(shifts: IndexedSeq[Int]) extends Component {

  val inputs = slave Flow (Vec(SInt(9 bits), 5))
  val output = master Flow SInt //  bitWidth can be determined later

  output.payload := shiftAdderTree(inputs.payload, shifts)
  output.valid := RegNext(inputs.valid)
  output.valid.init(False)
}

object ShiftAdderTree {

  // TODO: draw the schematic and document it
  def shiftAdderTree(inputs: IndexedSeq[SInt], shifts: IndexedSeq[Int]) = {
    require(inputs.length == shifts.length, "shiftAdderTree: number of operands and shifts must match")

    val sortedPairs = inputs.zip(shifts).sortBy { case (input, shift) => shift } //  ascending sequence

    // TODO: to prove and verify that this is just-right
    def buildTree(pairs: IndexedSeq[(SInt, Int)]): IndexedSeq[(SInt, Int)] = {
      val n = pairs.length
      if (n == 1) pairs
      else {
        val half = (n + 1) / 2
        val outputs = (0 until half)
          .map { i =>
            if ((i == half - 1) && n % 2 == 1) pairs(n - 1)
            else {
              val inputs = (pairs(2 * i)._1, pairs(2 * i + 1)._1)
              val shifts = (pairs(2 * i)._2, pairs(2 * i + 1)._2)
              val shift = shifts._2 - shifts._1
              (inputs._1 +^ (inputs._2 << shift), shifts._1)
            }
          }
        outputs.foreach { case (output, shift) => output.addAttribute("dont_touch = \"yes\"") }
        println(outputs.map(_._2).mkString(" "))
        buildTree(outputs)
      }
    }

    val resultPair = buildTree(sortedPairs)(0)
    println(s"bitGrowth = ${resultPair._1.getBitsWidth + resultPair._2 - inputs(0).getBitsWidth}")
    RegNext(resultPair._1 << resultPair._2)
  }

  def main(args: Array[String]): Unit = {
    VivadoFlow(
      design = new ShiftAdderTree(Array(1, 3, 5, 7, 8)),
      topModuleName = "shiftAdderTree",
      workspacePath = "output/shiftAdderTree",
      force = true).doit()
  }
}

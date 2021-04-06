package DSP

import spinal.core._ //  for digital signal processing

class ShiftAdderTree extends Component {

  val io = new Bundle {

  }

}

object ShiftAdderTree {

  // TODO: draw the schematic and document it
  def shiftAdderTree(inputs: IndexedSeq[SInt], shifts: IndexedSeq[Int]) = {
    require(inputs.length == shifts.length, "shiftAdderTree: number of operands and shifts must match")

    val sortedPairs = inputs.zip(shifts).sortBy { case (input, shift) => shift } //  ascending sequence

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
              (inputs._1 +^ inputs._2 << shift, shifts._1)
            }
          }
        outputs.foreach { case (output, shift) => output.addAttribute("dont_touch = \"yes\"") }
        buildTree(outputs)
      }
    }
  }

}

package Chainsaw

import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import Chainsaw._
import Chainsaw.Real

object Draft {

  implicit class SpinalLiterals(private val sc: StringContext) {


    /** Q Format literal, we follow the same definition as Xilinx, where MQN stands for a (M + N + 1) bits signed fixed point number with N fractional bits and M integer bits
     *
     * @see [[https://en.wikipedia.org/wiki/Q_(number_format) Q format]]
     */
    def QFormatParser(string: String): QFormat = {
      val isUnsigned = string.contains("UQ")
      val digits = {
        if (string.contains("SQ")) string.split("SQ").map(_.toInt)
        else if (string.contains("Q")) string.split("Q").map(_.toInt)
        else if (string.contains("UQ")) string.split("UQ").map(_.toInt)
        else throw new IllegalArgumentException("A QFormat must be like 1Q3, 1SQ3, or 1UQ3")
      }

      require(digits.length == 2 && digits.forall(_ >= 0))
      if (isUnsigned) UQ(digits(0) + digits(1), digits(1)) else SQ(digits(0) + digits(1) + 1, digits(1))

    }

    def Q(args: Any*): QFormat = QFormatParser(getString(args))

    private def getString(args: Any*): String = {

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next().toString)

      while (ai.hasNext) {
        if (ai.hasNext && !ai.next.isInstanceOf[List[_]]) bldr append ai.next
        if (pi.hasNext && !pi.next.isInstanceOf[List[_]]) bldr append pi.next
      }

      bldr.result.replace("_", "")
    }
  }

  def main(args: Array[String]): Unit = {
    println(Q"1Q4")
  }

}

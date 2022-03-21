package Chainsaw.comparith

import Chainsaw._
import spinal.core._
import spinal.lib._

// TODO: area or component?
// n-l compressor, follows the definition of
class Compressor(n: Int, l: Int, input: Bits) extends Area {
  require(n >= 2 && l >= log2Up(n + 1) && input.getBitsWidth == n)
  def recursion(n: Int, l: Int): Bits = {
    (n, l) match {
      case (2, 2) => { // HA
        val sum  = input(1) ^ input(0)
        val cOut = input(1) & input(0)
        Seq(sum, cOut).asBits()
      }
      case (3, 2) => { // FA
        val sum  = input.xorR
        val cOut = (input(0) & input(1)) | input(1) & input(2) | input(2) & input(0)
        Seq(sum, cOut).asBits()
      }
    }
  }
  val output = recursion(n, l)
}

case class FA(x: Bool, y: Bool, cIn: Bool) extends Compressor(3, 2, x ## y ## cIn) {
  val carry = output(1)
  val sum   = output(0)
}

case class HA(x: Bool, y: Bool) extends Compressor(2, 2, x ## y) {
  val carry = output(1)
  val sum   = output(0)
}

object BuildingBlocksExample {
  def main(args: Array[String]): Unit = {
    GenRTL(new Component {
      val x, y, cin = in Bool ()
      val sum, cout = out Bool ()

      val fa = FA(x, y, cin)
      sum  := fa.sum
      cout := fa.carry
    })
  }
}

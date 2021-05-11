package Chainsaw

import spinal.core._

import scala.util.Random //  for digital signal processing

class TimeSequence extends ImplicitArea[UInt] {

  def randomTimeSequence(length: Int): String = {
    val r = new Random()

    def op(code: Int) = {
      code match {
        case 0 => "+"
        case 1 => "-"
        case 2 => "*"
        case 3 => "/"
        case 4 => "%"
      }
    }

    "t" + (0 until length).map(_ => op(r.nextInt(5)) + r.nextInt(100).toString).mkString("")
  }

  override def implicitValue: UInt = U(0)
}

object TimeSequence {
  def main(args: Array[String]): Unit = {

  }
}

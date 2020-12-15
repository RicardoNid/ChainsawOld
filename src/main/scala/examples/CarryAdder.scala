package examples

import spinal.core._
import spinal.lib._

class CarryAdder(size: Int) extends Component {
  val io = new Bundle {
    val a = in UInt (size bits)
    val b = in UInt (size bits)
    val result = out UInt (size bits)
  }
  var c = False
  for (i <- 0 until size) {
    val a = io.a(i)
    val b = io.b(i)

    io.result(i) := a ^ b ^ c
    c \= (a & b) | (b & c) | (a & c)
  }
}

object CarryAdder {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new CarryAdder(4))
  }
}

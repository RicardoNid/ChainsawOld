package Xilinx

import spinal.core._

class AndGate extends Component {

  val io = new Bundle {
    val a = in UInt (8 bits)
    val b = in UInt (8 bits)
    val c = out UInt (8 bits)
  }

  var mid = UInt(8 bits)
  mid := io.a
  mid \= mid % 1948
  io.c := mid + io.b

}

object AndGate {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new AndGate)
  }
}
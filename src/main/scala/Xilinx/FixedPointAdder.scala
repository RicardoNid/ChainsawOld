package Xilinx

import spinal.core._

class FixedPointAdder extends Component {

  val io = new Bundle {
    val a = in UFix(8 exp, -2 exp)
    val b = in UFix(6 exp, -4 exp)
    val sum = out UFix(8 exp, -2 exp)
  }
  io.sum := (io.a + io.b).truncated
}

object FixedPointAdder {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new FixedPointAdder)
  }
}
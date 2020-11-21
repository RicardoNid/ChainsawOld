package mylib

import spinal.core._

class AdderCell extends Component {
  val io = new Bundle {
    val a, b, cin = in UInt(1 bits)
    val sum, cout = out Bool
  }
  val whole = UInt(2 bits)
  whole := io.a +^ io.b + io.cin
  io.sum := whole(0)
  io.cout := whole(1)
}

object AdderCell {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new AdderCell())
  }
}



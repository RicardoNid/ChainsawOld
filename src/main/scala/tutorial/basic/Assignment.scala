package tutorial.basic

import spinal.core._

class Assignment extends Component {

  val io = new Bundle {
    val input = in Bits(2 bits)
    val output0 = out Bits(4 bits)
    val output1 = out Bits(4 bits)
  }

  val x0, y, z = UInt(4 bits)
  var x1 = UInt(4 bits)

  x0 := 0
  x0 := x0 + 0
  y := x0

  x1 := 0
  x1 \= x1 +1
  z := x1

  io.output0 := y.asBits
  io.output1 := z.asBits

}

object Assignment {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "output/VerilogLike")
      .generateSystemVerilog(new Assignment)
  }
}

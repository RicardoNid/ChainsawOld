package tutorial.basic

import spinal.core._

class Datatype extends Component {

  val io = new Bundle {
    val input0 = in Bool
    val input1 = in Bits (10 bits)
    val input2 = in UInt (10 bits)
    val input3 = in SInt (10 bits)
    val output0 = out Bool
    val output1 = out Bits (10 bits)
    val output2 = out UInt (10 bits)
    val output3 = out SInt (10 bits)
  }

  io.output0 := !io.input0
  io.output1 := (io.input1.asUInt + io.input2).asBits
  io.output2 := io.input2 + io.input3.asUInt
  io.output3 := io.input3 + io.input2.asSInt
}

object Datatype {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "output/VerilogLike")
      .generateSystemVerilog(new Datatype)
  }
}

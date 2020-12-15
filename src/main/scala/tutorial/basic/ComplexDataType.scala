package tutorial.basic

import spinal.core._
import tutorial.basic.Riscv._

class ComplexDataType extends Component {

  val io = new Bundle {
    val input = in(MIPS())
    val output = out(MIPS())

    val anotherInput = in Bits(32 bits)
    val anotherOutput = out Bits(32 bits)
  }

  io.output := io.input
  io.output.opcode := ~io.input.opcode

  io.anotherOutput := io.anotherInput
  when(io.anotherInput === ADD)(io.anotherOutput(rdRange) := ~io.anotherInput(rdRange))
}

object ComplexDataType {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "output/VerilogLike")
      .generateSystemVerilog(new ComplexDataType)
  }
}

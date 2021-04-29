package FTN

import spinal.core._
import xilinx._

class BRAMTest extends Component {
  val input = in UInt (4 bits)
  val output = out UInt (8 bits)

  val memory = Mem(UInt(4 bits), 32)

  memory.write(U(0, memory.addressWidth bits), input)
  output := memory.readAsync(U(0, memory.addressWidth bits)).resized
}

object BRAMTest {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new BRAMTest)
    VivadoFlow(design = new BRAMTest, topModuleName = "BRAMTest", workspacePath = "output/BRAMTest", force = true).doit()
  }
}

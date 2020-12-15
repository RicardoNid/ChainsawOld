package temp

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class boot extends Component {

  val io = new Bundle {
    val clk, input = in Bool
    val result = out Bool
  }

  new ClockingArea(new ClockDomain(
    clock = io.clk,
    config = ClockDomainConfig(resetKind = BOOT))) {
    val resultReg = RegInit(False)
    resultReg := io.input

    io.result := resultReg
  }


}

object boot {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "output/temp").generateSystemVerilog(new boot)
  }
}

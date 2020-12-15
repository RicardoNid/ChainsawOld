package VerilogLike

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class SimpleModule extends Component {

  val io = new Bundle {
    val input = in Bool
    val output = out Bool
  }

  //
  val outputReg = RegInit(False)
  outputReg := (~io.input | io.input)
  io.output := outputReg

}

object SimpleModule {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new SimpleModule)
  }
}

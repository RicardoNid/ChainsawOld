package tutorial.basic

import spinal.core._
import spinal.lib._
import spinal.core.sim._

class Registers extends Component {

  val io = new Bundle {
    val input = in UInt (4 bits)
    val pass = in Bool()
    val output1 = out UInt (4 bits)
    val output2 = out UInt (4 bits)
    val output3 = out UInt (4 bits)
    val output4 = out UInt (4 bits)
  }

  val temp1 = Reg(UInt(4 bits))
  temp1 := io.input
  temp1.init(U(12))
  when(io.pass) {
    temp1 := io.input
  }
  io.output1 := temp1

  val temp2 = RegInit(U(12))
  temp2 := io.input
  when(io.pass) {
    temp2 := io.input
  }
  io.output2 := temp2

  // implement the functions above by RegNext, RegNextWhen


}

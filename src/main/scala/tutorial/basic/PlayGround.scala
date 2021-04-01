package tutorial.basic

import spinal.core._

class PlayGround(add: Boolean) extends Component {

  val io = new Bundle {
    val input0 = in UInt (4 bits)
    val input1 = in UInt (4 bits)
    val output0 = out UInt (4 bits)
  }

  if (add) io.output0 := io.input0 + io.input1
  else io.output0 := io.input0 - io.input1

}

object PlayGround {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new PlayGround(true))
  }
}

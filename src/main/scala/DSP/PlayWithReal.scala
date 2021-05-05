package DSP

import spinal.core._

class PlayWithReal extends Component {

  //  val input = in SReal (RealRange(3.0, -3.0, 0.01))
  //
  //  val mid = input + input
  //
  //  val output = out SReal (RealRange(3.0, -3.0, 0.01))

  object stateEnum extends SpinalEnum {
    val s0, s1, s2 = newElement()
  }

  val input = in Bool()

  //  when(input === s0)

  //  output := mid.truncated


}

object PlayWithReal {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new PlayWithReal)
  }
}

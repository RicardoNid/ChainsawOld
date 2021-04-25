package DSP

import spinal.core._

class PlayWithSFix extends Component {
  val input = in Vec(SFix(3.99, -2.0, 0.01), 2)
  val output = out SFix(8.0, -4.0, 0.01)

  output := input(0) + input(1)
}

object PlayWithSFix {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new PlayWithSFix)
  }
}



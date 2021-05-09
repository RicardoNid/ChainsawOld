package tutorial.examples

import spinal.core._

class Resize extends Component {
  val a = in SInt (5 bits)
  val b = out SInt (4 bits)

  b := a.resized
}
object Resize {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new Resize)
  }
}

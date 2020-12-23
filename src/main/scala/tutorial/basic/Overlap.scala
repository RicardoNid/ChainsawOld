package tutorial.basic

import spinal.core._

class Overlap extends Component {
    val a = UInt(8 bits)
    a := 42
    a := 66
}

object Overlap {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new Overlap)
  }
}

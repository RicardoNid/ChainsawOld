package basic

import spinal.core._

class SevenSeg extends Component {
  val io = new Bundle {
    val a = in Vec(UInt(4 bits), 4)
    val b = in Vec(UInt(4 bits), 4)
    val c = out Vec(UInt(4 bits), 4)
  }
}

object SevenSeg {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new SevenSeg)
  }
}

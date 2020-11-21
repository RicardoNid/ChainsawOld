package Xilinx

import spinal.core._

class Timer(width: Int) extends Component {
  val io = new Bundle {
    val tick = in Bool
    val clear = in Bool
    val limit = in UInt (width bits)
    val full = out Bool
  }

  val counter = Reg(UInt(width bits))

  when(io.clear) {
    counter := 0
  }.elsewhen(io.tick && !io.full) {
    counter := counter + 1
  }
  io.full := counter === io.limit
}

object Timer {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Timer(10))
  }
}



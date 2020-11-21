package Xilinx

import spinal.core._

class DualPortRAM extends Component {

  val io = new Bundle {
    val writeValid = in Bool
    val writeAddress = in UInt (8 bits)
    val writeData = in Bits (32 bits)

    val readValid = in Bool
    val readAddress = in UInt (8 bits)
    val readData = out Bits (32 bits)

    val clockRd = in Bool
    val clockWr = in Bool
  }

  val domainRd = ClockDomain(io.clockRd)
  val domainWr = ClockDomain(io.clockWr)

  val mem = Mem(Bits(32 bits), wordCount = 256)

  val areaRd = new ClockingArea(domainRd) {
    io.readData := mem.readSync(
      enable = io.readValid,
      address = io.readAddress
    )
  }

  val areaWr = new ClockingArea(domainWr) {
    mem.write(
      enable = io.writeValid,
      address = io.writeAddress,
      data = io.writeData
    )
  }
}

object DualPortRAM {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new DualPortRAM)
  }
}

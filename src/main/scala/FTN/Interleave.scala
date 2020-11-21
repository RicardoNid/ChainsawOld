package FTN

import spinal.core._
import spinal.lib._

class Interleave(bitWidth: Int, sizeRd: Int, sizeWr: Int) extends Component {

  def typeWord = UInt(bitWidth bits)

  val depth = sizeRd * sizeWr

  val io = new Bundle {
    val clockInput = in Bool
    val clockOutput = in Bool
    val rstInput = in Bool
    val rstOutput = in Bool
    val input = in UInt (bitWidth bits)
    val output = out UInt (bitWidth bits)
  }

  val ping = Mem(typeWord, sizeRd * sizeWr)
  val pong = Mem(typeWord, sizeRd * sizeWr)


  val domainWr = ClockDomain(clock = io.clockInput)
  val domainRd = ClockDomain(clock = io.clockOutput)

  val areaRd = new ClockingArea(ClockDomain(clock = io.clockOutput, reset = io.rstOutput)) {
    val counterRd = Counter(0, depth, True)
    val countRd = counterRd.value
    val addressRd = UInt(log2Up(depth) bits)
    addressRd := ((countRd % U(sizeRd)) * U(sizeWr) + countRd / U(sizeRd)).resized
    io.output := ping.readSync(addressRd, clockCrossing = true)
  }

  val areaWr = new ClockingArea(ClockDomain(clock = io.clockInput, reset = io.rstInput)) {
    val counterWr = Counter(0, depth, True)
    val countWr = counterWr.value
    val addressWr = UInt(log2Up(depth) bits)
    addressWr := ((countWr % U(sizeWr)) * U(sizeRd) + countWr / U(sizeWr)).resized // fixme : 通过一些技巧可以消除除法和取模
    ping(addressWr) := io.input
  }
  noIoPrefix()
}

object Interleave {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new Interleave(8, 64, 32))
  }
}

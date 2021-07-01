package Chainsaw.Crypto.RSA.history

import Chainsaw._
import spinal.core._
import spinal.lib._

case class DPRAM(w: Int) extends Component {
  require(isPow2(w))
  val lgw = log2Up(w)
  val output = out UInt (1 bits)
  val memory = Mem((0 until 16).map(U(_, w bits)))
  val xCounter = CounterFreeRun(w * 16)
  val addr = xCounter.value >> log2Up(w)
  val bitAddr = xCounter.value(lgw - 1 downto 0)
  output := memory(addr)(bitAddr).asUInt
}

object DPRAM {
  def main(args: Array[String]): Unit = {
    GenRTL(DPRAM(32))
    VivadoSynth(new DPRAM(32))
  }
}

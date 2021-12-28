package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class TestROM() extends Component {

  val dataOut = out Bits(768 bits)

  val value = (0 until 64).map(i => (i - 32))
  val hardValue = (0 until 16).map(_ => Vec((value ++ value.reverse).map(int => SF(int, 5 exp, 0 exp))))
  val rom: Mem[Vec[SFix]] = Mem(hardValue)

  val counter = CounterFreeRun(16)
  dataOut := rom.readAsync(counter.value).asBits
}

object TestROM extends App {
  GenRTL(TestROM(),name = "ROM")
}

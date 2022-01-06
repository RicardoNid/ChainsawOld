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

  val scaling = 0.3
  val value = (0 until 64).map(i => (i * scaling).toInt)
  val hardValue = (0 until 16).map(_ => Vec((value ++ value.reverse).map(int => U(int, 6 bits))))
  val rom: Mem[Vec[UInt]] = Mem(hardValue)

  val counter = CounterFreeRun(16)
  dataOut := rom.readAsync(counter.value).asBits
}

object TestROM extends App {
  GenRTL(TestROM(),name = "TestROM")
}

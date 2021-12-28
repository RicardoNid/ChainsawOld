package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class TestBuffer() extends Component {

  val dataIn = in Bits (768 bits)
  val counter = CounterFreeRun(16)

  val ram = Mem(cloneOf(dataIn), 16)
  ram.write(counter.value, dataIn)

}

object TestBuffer extends App {
  GenRTL(TestBuffer(), name = "TestBuffer")
}



package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

case class AddressingExamples() extends Component {

  val counter = CounterFreeRun(10)
  val counterAddr = CounterFreeRun(16)

  val rams = Seq.fill(10)(Mem(UInt(4 bits), 16))

  val ramOutputs = Vec(rams.map(_.readSync(counterAddr.value)))
  val ret = ramOutputs(counter.value)

}

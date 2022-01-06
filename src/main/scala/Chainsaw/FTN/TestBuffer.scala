package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class TestBuffer() extends Component {

  val dataIn = slave Stream Bits(768 bits)
  val dataOut = master Stream Bits(768 bits)

  val clockConfig = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW)
  val ioClock = ClockDomain.external("io", clockConfig, frequency = FixedFrequency(230.4 MHz))
  val baseClock = ClockDomain.external("base", clockConfig, frequency = FixedFrequency(288 MHz))

  val dataType = HardType(Bits(768 bits))
  val asyncFifo = StreamFifoCC(dataType, 128, ioClock, baseClock)

  dataIn >> asyncFifo.io.push
  asyncFifo.io.pop >> dataOut
}

object TestBuffer extends App {
  GenRTL(TestBuffer(), name = "TestBuffer")
}



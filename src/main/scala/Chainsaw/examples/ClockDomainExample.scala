package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

class ClockDomainExample extends Component {

  val dataType = HardType(UInt(4 bits))
  val dataIn, dataOut = dataType()
  in(dataIn)
  out(dataOut)

  val domain1 = ClockDomain.external("domain1", config = ClockDomainConfig(resetKind = ASYNC), frequency = FixedFrequency(300 MHz))
  val domain2 = ClockDomain.external("domain2", config = ClockDomainConfig(resetKind = ASYNC), frequency = FixedFrequency(500 MHz))

  StreamFifoCC(UInt(4 bits), 16, pushClock = domain1, popClock = domain2)

  val slow = new ClockingArea(domain1) {
    val regSlow = Reg(dataType)
    regSlow := dataIn
  }

  val high = new ClockingArea(domain2) {
    val regHigh = Reg(dataType)
    dataOut := regHigh
  }

  val inter = dataType()
  inter.addTag(crossClockDomain)

  inter := slow.regSlow
  high.regHigh := inter

}

object ClockDomainExample extends App {
  GenRTL(new ClockDomainExample)
}

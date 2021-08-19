package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._


import Chainsaw._
import Chainsaw.Real

case class AsyncFIFOExample() extends Component {

  val dataType = HardType(UInt(8 bits))
  val io = new Bundle {
    val dataIn = slave Stream dataType
    val dataOut = master Stream dataType
  }

  val cdPushConfig = ClockDomainConfig(clockEdge = RISING, resetKind = SYNC, resetActiveLevel = HIGH, softResetActiveLevel = HIGH, clockEnableActiveLevel = HIGH)
  val cdPopConfig = ClockDomainConfig(clockEdge = RISING, resetKind = SYNC, resetActiveLevel = HIGH, softResetActiveLevel = HIGH, clockEnableActiveLevel = HIGH)
  val cdPush = ClockDomain.external(name = "pop", config = cdPushConfig, frequency = FixedFrequency(200 MHz))
  val cdPop = ClockDomain.external(name = "pop", config = cdPopConfig, frequency = FixedFrequency(400 MHz))

  val fifo = StreamFifoCC(dataType = dataType, depth = 16, pushClock = cdPush, popClock = cdPop)

  io.dataIn >> fifo.io.push
  fifo.io.pop >> io.dataOut
}

object AsyncFIFOExample extends App {
  GenRTL(new AsyncFIFOExample())
}

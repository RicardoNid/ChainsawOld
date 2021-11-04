package Chainsaw.memories

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class CrossClockExample(exampleId: Int) extends Component {
  val io = new Bundle {
    val input = in UInt (4 bits)
    val output = out UInt (4 bits)
  }

  if(exampleId == 0){ // example using default clockdomain
    globalData.commonClockConfig = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = SYNC,
      resetActiveLevel = LOW)

    println(ClockDomain.current.config)
    println(clockDomain.config)

    io.output := RegNext(io.input, init = U(0))
  }

  if (exampleId == 1) { // example using a new external domain
    val externalDomain = ClockDomain.external(
      name = "external",
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = SYNC,
        resetActiveLevel = LOW
      )
    )
    new ClockingArea(externalDomain){
      io.output := RegNext(io.input, init = U(0))
    }
  }

  if(exampleId == 2){ // example of the resetKind = BOOT
    val externalDomain = ClockDomain.external(
      name = "external",
      config = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = BOOT,
        resetActiveLevel = LOW
      )
    )
    new ClockingArea(externalDomain){
      io.output := RegNext(io.input, init = U(0))
    }
  }
}

object CrossClockExample {
  def main(args: Array[String]): Unit = {
    GenRTL(new CrossClockExample(0))
    GenRTL(new CrossClockExample(1))
    GenRTL(new CrossClockExample(2), name = "usingBOOT")
  }
}

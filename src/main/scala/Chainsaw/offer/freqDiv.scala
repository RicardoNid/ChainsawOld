package Chainsaw.offer

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class freqDiv(factor: Int) extends Component {
  val io = new Bundle {
    val clkIn = in Bool()
    val rst = in Bool()
    val clkOut = out Bool()
  }

  val risingDomain = ClockDomain(clock = io.clkIn, reset = io.rst)
  val fallingConfig = ClockDomainConfig(clockEdge = FALLING)
  val fallingDomain = ClockDomain(clock = io.clkIn, reset = io.rst, config = fallingConfig)

  val risingArea = new ClockingArea(risingDomain) {
    val risingCounter = CounterFreeRun(factor)
    //    val clkP = risingCounter.value >= (factor / 2 + 1)
    val clkP = risingCounter.value < (factor / 2)
  }

  val fallingArea = new ClockingArea(fallingDomain) {
    val fallingCounter = CounterFreeRun(factor)
    //    val clkN = fallingCounter.value >= (factor / 2 + 1)
    val clkN = fallingCounter.value < (factor / 2)
  }

  io.clkOut.addTag(crossClockDomain)
  io.clkOut := risingArea.clkP || fallingArea.clkN
}

object freqDiv extends App {
  //  VivadoSynth(new freqDiv(5))
  SimConfig.withWave.compile(new freqDiv(5)).doSim { dut =>
    dut.risingDomain.forkStimulus(2)
    dut.risingDomain.waitSampling(20)
    dut.io.rst #= true
    sleep(10)
    dut.io.rst #= false
    dut.risingDomain.waitSampling(20)
  }
}

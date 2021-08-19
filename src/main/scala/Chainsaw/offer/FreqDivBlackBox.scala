package Chainsaw.offer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class FreqDivBlackBox() extends BlackBox {

  val clk_in = in Bool()
  val rst = in Bool()
  val clk_out = out Bool()

  noIoPrefix()
  mapCurrentClockDomain(clk_in, rst)

  setDefinitionName("FreqDiv")
  addRTLPath("FreqDiv.sv")
}

case class FreqDivDut() extends Component {

  val dut = FreqDivBlackBox()

  val clk_out = out Bool()
  clk_out := dut.clk_out
}

object TestFreqDiv extends App {
  SimConfig.addRtl("FreqDiv.sv").withWave.compile(new FreqDivDut()).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling(20)
    dut.clockDomain.assertReset()
    sleep(10)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitSampling(20)
  }
}
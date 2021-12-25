package Chainsaw.examples

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import breeze.linalg._
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

/**
 * @param initValue matrix of bits
 */
case class DataGen(initValue: DenseMatrix[Int]) extends Component {

  val bitWidth = initValue.cols
  val period = initValue.rows

  val dataOut = master Stream (Bits(bitWidth bits))
  val hardValues: Seq[Bits] = initValue.t.data // change order
    .grouped(bitWidth).toSeq // get rows
    .map(_.mkString("")).map(B(_))

  val rom = Mem(hardValues)

  val counter = CounterFreeRun(period)

  dataOut.payload := rom.readAsync(counter)
  dataOut.valid := True
}

case class WidthAdapt() extends Component with DSPTestable[Bits, Bits] {

  val baseWidth = 4
  val ADDAWidth = 6

  override val dataIn = slave Stream Bits(baseWidth bits)
  override val dataOut = master Stream Bits(ADDAWidth bits)
  override val latency = 1

  val counter = CounterFreeRun(3)
  val regA = Reg(Bits(baseWidth bits))

  val zero = dataOut.payload.getZero

  switch(counter.value) {
    is(U(0)) {
      dataOut.payload := zero
      regA := dataIn.payload
    }
    is(U(1)) {
      dataOut.payload := regA ## dataIn.payload.takeHigh(baseWidth / 2)
      regA := dataIn.payload
    }
    is(U(2)) {
      dataOut.payload := regA.takeLow(baseWidth / 2) ## dataIn.payload
    }
    default(dataOut.payload := zero)
  }

  dataIn.ready := True
  dataOut.valid := counter === U(1) || counter === U(2)

}

case class FTNDataGen() extends Component {

  val baseWidth = 4
  val ADDAWidth = 6

  val clk2304 = ClockDomain.external("clk230M4", frequency = FixedFrequency(230.4 MHz))
  val clk3072 = ClockDomain.external("clk307M2", frequency = FixedFrequency(307.2 MHz))

  val doubleData = DenseMatrix.rand[Double](32, baseWidth) + 0.5 // two frames
  val intData = convert(doubleData, Int)

  //  val intData = DenseMatrix.zeros[Int](32, baseWidth)
  //  (0 until 32).foreach(i => intData(i, i) = 1)

  val fastArea = new ClockingArea(clk3072) {
    val dataGen = DataGen(intData)
    val widthAdapt = WidthAdapt()
    dataGen.dataOut >> widthAdapt.dataIn
  }

  val dataOut = master Stream Bits(ADDAWidth bits)
  val asyncFIFO = StreamFifoCC(Bits(ADDAWidth bits), 16, clk3072, clk2304)
  fastArea.widthAdapt.dataOut >> asyncFIFO.io.push
  asyncFIFO.io.pop >> dataOut
}

object FTNDataGen extends App {
  GenRTL(FTNDataGen(), name = "DataGen")

  SimConfig.withWave.compile(FTNDataGen()).doSim { dut =>
    dut.dataOut.ready #= true
    dut.clk2304.forkStimulus(3)
    dut.clk3072.forkStimulus(3)
    dut.clk2304.waitSampling(4096)
  }

}

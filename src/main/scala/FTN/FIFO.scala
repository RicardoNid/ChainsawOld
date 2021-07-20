package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

// test the function and performance of FIFO and Stack
class FIFO extends Component {
  def dataType = Bits(4 bits)
  val innerFIFO = StreamFifo(dataType, 8)
  val innerFIFOLowLatency = StreamFifoLowLatency(dataType, 8)
  val inPort = slave Stream dataType
  val outPort = master Stream dataType
  inPort >> innerFIFO.io.push
  innerFIFO.io.pop >> outPort
}

object FIFO {
  def main(args: Array[String]): Unit = {
    GenRTL(new FIFO)
    SimConfig.withWave.compile(new FIFO).doSim { dut =>
      dut.inPort.valid #= true
      dut.outPort.ready #= true
      dut.clockDomain.forkStimulus(2)

      (0 until 8).foreach { i =>
          dut.inPort.payload #= i
          dut.clockDomain.waitSampling()
      }
    }
  }
}



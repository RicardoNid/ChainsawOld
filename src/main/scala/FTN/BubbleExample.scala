package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import spinal.core.Component.{pop, push}

class BubbleExample extends Component {
  val fifo0 = StreamFifo(UInt(4 bits), 5)
  val fifo1 = StreamFifo(UInt(4 bits), 5)
  val fifo2 = StreamFifo(UInt(4 bits), 5)
  val fifo3 = StreamFifo(UInt(4 bits), 5)
  val fifo4 = StreamFifo(UInt(4 bits), 5)
  val fifo5 = StreamFifo(UInt(4 bits), 5)

  fifo1.io.push <-< fifo0.io.pop

  val m2s = fifo2.io.pop.m2sPipe(false)
  fifo3.io.push <> m2s

  val halfPipe = fifo4.io.pop.halfPipe()
  fifo5.io.push <> halfPipe

  // set input
  val inPort = Vec(slave Stream (UInt(4 bits)), 3)
  inPort(0) <> fifo0.io.push
  inPort(1) <> fifo2.io.push
  inPort(2) <> fifo4.io.push


  // set output
  val outPort = Vec(master Stream (UInt(4 bits)), 3)
  outPort(0) <> fifo1.io.pop
  outPort(1) <> fifo3.io.pop
  outPort(2) <> fifo5.io.pop
}

object BubbleExample {
  def main(args: Array[String]): Unit = {
    GenRTL(new BubbleExample)

    SimConfig.withWave.compile(new BubbleExample).doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      dut.inPort.foreach(_.valid #= true)
      dut.outPort.foreach(_.ready #= false)
      dut.clockDomain.waitSampling()

      (0 until 5).foreach { i =>
        dut.inPort.foreach(_.payload #= i)
        dut.clockDomain.waitSampling()
      }

      dut.inPort.foreach(_.valid #= false)
      dut.outPort.foreach(_.ready #= false)
      dut.clockDomain.waitSampling(10)

      dut.inPort.foreach(_.valid #= true)
      dut.outPort.foreach(_.ready #= false)
      (0 until 5).foreach { i =>
        dut.inPort.foreach(_.payload #= i)
        dut.clockDomain.waitSampling()
      }

      dut.outPort.foreach(_.ready #= true)
      (0 until 15).foreach{ i =>
        dut.inPort.foreach(_.payload #= i)
        dut.clockDomain.waitSampling()
      }

      dut.clockDomain.waitSampling(15)
    }
  }
}


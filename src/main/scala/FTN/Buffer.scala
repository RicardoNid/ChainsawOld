package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

class Buffer extends Component {
  //  val input = slave Stream Bits(4 bits)
  val input = Stream(Bits(4 bits))
  input.asSlave()
  val fifo = StreamFifo(Bits(4 bits), 8)
  fifo.io.push <> input
  val output = master Stream Bits(4 bits)
  fifo.io.pop <> output


}

object Buffer {
  def main(args: Array[String]): Unit = {
    GenRTL(new Buffer)
    VivadoSynth(new Buffer)
    //    SimConfig.withWave.compile(new Buffer).doSim { dut =>
    //
    //      dut.clockDomain.forkStimulus(2)
    //
    //      dut.input.valid #= true
    //      dut.output.ready #= true
    //
    //      (0 until 16).foreach { i =>
    //        dut.input.payload #= i
    //        dut.clockDomain.waitSampling()
    //      }
    //
    //      dut.input.valid #= true
    //      dut.output.ready #= false
    //
    //      (0 until 16).foreach { i =>
    //        dut.input.payload #= i
    //        dut.clockDomain.waitSampling()
    //      }
    //
    //      dut.input.valid #= false
    //      dut.output.ready #= true
    //
    //      (0 until 16).foreach { i =>
    //        dut.input.payload #= i
    //        dut.clockDomain.waitSampling()
    //      }
    //
    //      dut.input.valid #= false
    //      dut.output.ready #= false
    //
    //      (0 until 16).foreach { i =>
    //        dut.input.payload #= i
    //        dut.clockDomain.waitSampling()
    //      }
    //    }
  }
}

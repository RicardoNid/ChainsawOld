package Chainsaw.comm.channelEqualizer

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class MuxExample() extends Component {

  val a, b = in Bits (18 bits)
  val c = out Bits (18 bits)


  val counter = CounterFreeRun(31)
  switch(counter.value) {
    (0 until 31).foreach(i => is(U(i))(if (i % 2 == 0) c := a else c := b))
    default(c := a)
  }

}

object MuxExample {
  def main(args: Array[String]): Unit = {
    VivadoSynth(MuxExample())
//    SimConfig.withWave.compile(MuxExample()).doSim{dut =>
//      dut.a #= 128
//      dut.b #= 63
//      dut.clockDomain.forkStimulus(2)
//      dut.clockDomain.waitSampling(100)
//    }

  }
}

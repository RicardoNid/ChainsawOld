package Chainsaw.examples.Interfaces

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class S2P2S() extends Component {

  val dataIn = slave(Flow Fragment UInt(4 bits))
  val inner = Flow Fragment Vec(UInt(4 bits), 4)
  val dataOut = master(Flow Fragment UInt(4 bits))

  def holdData[T <: Data](start: Bool, data: T, cycles: Int) = {
    val hold = History(start, cycles, init = False).orR
    val heldData = RegNextWhen(data, start)
    val ret = Mux(start, data, heldData)
    (hold, ret)
  }

  val (hold, heldData) = holdData(inner.valid, inner.payload, 4)
  val innerNext = Flow Fragment Vec(UInt(4 bits), 4)
  innerNext.payload := heldData
  innerNext.valid := hold

  import BetterFlow._

  FlowWidthAdapter(dataIn, inner)
  FlowWidthAdapter(innerNext, dataOut)

}

object S2P2S {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(S2P2S()).doSim { dut =>
      import dut.{clockDomain, dataIn}
      dataIn.halt()
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()

      (0 until 8).foreach { i =>
        dataIn.poke(BigInt(i), i % 4 == 3)
        clockDomain.waitSampling()
      }
      dataIn.halt()
      clockDomain.waitSampling(20)
    }
  }
}

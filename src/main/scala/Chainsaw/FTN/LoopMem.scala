package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class LoopMem[T <: Data](wordType: HardType[Vec[T]], length: Int)
  extends Component {

  val we = in Bool()
  val dataIn = in(wordType())
  val dataOut = out(wordType())

  val counter = CounterFreeRun(length)
  val mem = BigMem(wordType, length)
  val dataTail = wordType()

  mem.write(counter.value, dataIn, we)
  dataTail := mem.readSync(counter.valueNext)
  dataOut := dataTail

  val latency = length // dataIn -> dataOut
}

object LoopMem extends App {

  val seq = (0 until 16).map(Seq(_))
  println(s"seq: ${seq.map(_.formatted("%h")).mkString(" ")}")

  val dataType = HardType(Vec(UInt(4 bits), 1))

  // simple test for loopMem
  SimConfig.withFstWave.compile(LoopMem(dataType, length = 16)).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling()

    seq.foreach { value =>
      dut.we #= true
      dut.dataIn.zip(value).foreach{ case (int, i) => int #= i}
      dut.clockDomain.waitSampling()
    }

    dut.we #= false
    dut.clockDomain.waitSampling(16)

    seq.foreach { value =>
      dut.we #= true
      dut.dataIn.zip(value).foreach{ case (int, i) => int #= i / 2}
      dut.clockDomain.waitSampling()
    }

    dut.we #= false
    dut.clockDomain.waitSampling(32)
  }
}

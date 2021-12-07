package Chainsaw.comm.viterbi

import org.scalatest.flatspec._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

class ViterbiHardTest extends AnyFlatSpec {

  val (trellis, testCases, golden) = Refs.getTestData
  val temp = Algos.viterbiForwarding(testCases, trellis, Algos.Hamming).head.zipWithIndex.minBy(_._1)._2

  def runSim() = {
    var dutResult = Seq[BigInt]()
    SimConfig.withWave.compile(ViterbiHard(trellis, 100, temp)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dutResult = flowPeekPoke(dut, testCases.map(BigInt(_)), dataIn, dataOut, dut.latency).asInstanceOf[Seq[BigInt]]
    }
    dutResult
  }

  "ViterbiHard" should "have the correct output" in {
    val dutResults = runSim().mkString("")
    assert(golden.reverse.mkString("") == dutResults.mkString(""))
  }

  it should "synth correctly" in {
    VivadoSynth(ViterbiHard(trellis, 100, temp))
  }

}

package Chainsaw.comm.viterbi

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ViterbiBackwardingTest extends AnyFlatSpec {

  val (trellis, forwardingTestcases, golden) = Refs.getTestData802_11n(128)
  val testCase: mutable.Stack[Seq[Double]] = Algos.viterbiForwarding(forwardingTestcases, trellis, Algos.Hamming)
  val testCaseCopy = Algos.viterbiForwarding(forwardingTestcases, trellis, Algos.Hamming)

  def runSim() = {

    val dutResults = ArrayBuffer[BigInt]()
    SimConfig.withWave.compile(ViterbiBackwarding(trellis)).doSim { dut =>
      import dut.{dataIn, dataOut, clockDomain}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling()

      def limit(value: Seq[Double]) = value.map(d => if (d > 15) 15 else d.toInt)

      //      setMonitor(True, dut.currentState, stateRecords) // TODO: fix this
      dataOut.setMonitor(dutResults)

      val end = testCase.length - 1

      testCase.indices.foreach { i =>
        dataIn.valid #= true
        dataIn.last #= (i == end)
        dut.stateStart #= (if (i == 0) testCase.head.zipWithIndex.minBy(_._1)._2 else 0)

        val record = testCase.pop()
        dataIn.fragment.zip(limit(record)).foreach { case (port, data) => port #= data }
        clockDomain.waitSampling()
      }

      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling(10)
    }
    dutResults
  }

  "ViterbiBackwarding" should "produce input symbols" in {
    val dutResults = runSim()
    val states = Algos.viterbiBackwarding(testCaseCopy, trellis).reverse // to map with trace back order
    val bits = states.map(_.toBinaryString.padToLeft(log2Up(trellis.numStates), '0').head.asDigit)
    assert(bits.mkString("") == dutResults.mkString(""))
  }

}

package Chainsaw.Communication.viterbi

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

class ViterbiForwardingTest extends AnyFlatSpec {

  val (trellis, testCases, golden) = Refs.getTestData

  def runSim() = {
    var dutResults = Seq[BigInt]()
    SimConfig.withWave.compile(ViterbiForwarding(trellis)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut, disNextLatency}
      clockDomain.forkStimulus(2)
      dutResults = flowPeekPokeRound(dut, testCases.map(BigInt(_)), dataIn, dataOut, dut.disNextLatency).asInstanceOf[Seq[BigInt]]
    }
    val dutString = dutResults.grouped(trellis.numStates).toSeq.map(_.map(i => if (i >= 8) '-' else i.toString()).mkString(" ")).mkString("\n")
    dutString
  }

  "ViterbiForwarding" should "produce next discrepancies" in {
    val dutString = runSim()
    val stack = Algos.viterbiForwarding(testCases, trellis, Algos.Hamming, 0)
    val goldenString = stack.reverse.map(_.map(_.toInt).map(i => if (i >= 127) '-' else i.toString).mkString(" ")).mkString("\n")
    assert(dutString.takeRight(trellis.numStates * 10) == goldenString.takeRight(trellis.numStates * 10))
  }

}

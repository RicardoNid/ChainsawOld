package Chainsaw.Communication.viterbi

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

class ViterbiForwardingTest extends AnyFlatSpec {

  val constLen = 7
  val codeGen = Array(177, 131)
  val trellis = Trellis.poly2trellis(constLen, codeGen)
  val trellisM = Refs.poly2trellisM(constLen, codeGen)

  val inputData = (0 until 100).map(_ => DSPRand.nextInt(trellis.numInputSymbols)).toArray
  val codedData = Refs.convenc(inputData, trellisM)
  val testCases = codedData.grouped(log2Up(trellis.numOutputSymbols)).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
  val golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)

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

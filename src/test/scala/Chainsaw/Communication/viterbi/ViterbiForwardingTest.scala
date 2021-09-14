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
  val testCase = codedData.grouped(log2Up(trellis.numOutputSymbols)).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
  val golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)

  def runSim() = {
    SimConfig.withWave.compile(ViterbiForwarding(trellis)).doSim { dut =>
      import dut.{clockDomain, dataIn, disNextLatency}
      clockDomain.forkStimulus(2)
      dataIn.halt()
      clockDomain.waitSampling()

      testCase.indices.foreach { i =>
        dataIn.poke(BigInt(codedData(i)), lastWhen = i == testCase.length - 1)
        clockDomain.waitSampling()
      }

      dataIn.halt()
      clockDomain.waitSampling(disNextLatency + 10)
    }
  }

  "ViterbiForwarding" should "produce next discrepancies" in {
    runSim()
    val stack = Algos.viterbiForwarding(testCase, trellis, Algos.Hamming, 0)
    println(stack.reverse.map(_.map(_.toInt).map(i => if (i >= 127) '-' else i.toString).mkString(" ")).mkString("\n"))
  }

}

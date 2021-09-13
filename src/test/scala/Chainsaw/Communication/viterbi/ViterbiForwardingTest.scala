package Chainsaw.Communication.viterbi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._

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
      import trellis._
      import dut.{dataIn, dataOut, clockDomain, disNextLatency}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling()

      testCase.init.foreach { coded =>
        dataIn.valid #= true
        dataIn.last #= false
        dataIn.fragment #= coded
        clockDomain.waitSampling()
      }

      dataIn.valid #= true
      dataIn.last #= true
      dataIn.fragment #= testCase.last
      clockDomain.waitSampling()

      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling(disNextLatency + 10)
    }
  }

  "ViterbiForwarding" should "produce next discrepancies" in {
    runSim()
    val stack = Algos.viterbiForwarding(testCase, trellis, Algos.Hamming, 0)
    println(stack.reverse.map(_.map(_.toInt).map(i => if (i >= 127) '-' else i.toString).mkString(" ")).mkString("\n"))
  }

}

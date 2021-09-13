package Chainsaw.Communication.viterbi

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class ViterbiBackwardingTest extends AnyFlatSpec {

  val constLen = 7
  val codeGen = Array(177, 131)
  val trellis = Trellis.poly2trellis(constLen, codeGen)
  val trellisM = Refs.poly2trellisM(constLen, codeGen)

  val inputData = (0 until 100).map(_ => DSPRand.nextInt(trellis.numInputSymbols)).toArray
  val codedData = Refs.convenc(inputData, trellisM)
  val decimal = codedData.grouped(log2Up(trellis.numOutputSymbols)).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray

  val testCase: mutable.Stack[Seq[Double]] = Algos.viterbiForwarding(decimal, trellis, Algos.Hamming)
  println(s"no bigger than 15: ${testCase.forall(_.forall(_ <= 15))}")
  val testCaseCopy = Algos.viterbiForwarding(decimal, trellis, Algos.Hamming)

  def runSim() = {
    SimConfig.withWave.compile(ViterbiBackwarding(trellis)).doSim { dut =>
      import dut.{dataIn, dataOut, clockDomain}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling()


      def limit(value: Seq[Double]) = value.map(d => if (d > 15) 15 else d.toInt)

      testCase.indices.foreach { i =>
        dataIn.valid #= true
        dataIn.last #= (i == testCase.length - 1)
        dut.stateStart #= (if (i == 0) testCase.head.zipWithIndex.minBy(_._1)._2 else 0)

        val record = testCase.pop()
        printlnGreen(record.mkString(" "))
        dataIn.fragment.zip(limit(record)).foreach { case (port, data) => port #= data }
        clockDomain.waitSampling()
      }

      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling(10)
    }
  }

  "ViterbiBackwarding" should "produce input symbols" in {
    runSim()
    val states = Algos.viterbiBackwarding(testCaseCopy, trellis).reverse // to map with trace back order
    println(states.mkString(" "))
    val bits = states.init.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    println(bits.mkString(""))
  }

}

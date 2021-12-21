package Chainsaw.comm.viterbi

import org.scalatest.flatspec.AnyFlatSpec

import Chainsaw._
import spinal.core._

class ViterbiAlgosTest extends AnyFlatSpec {

  // TODO: expand this to multi-in, multi-out

  // 801.11n standard
  val constLen = 7
  val codeGen = Array(177, 131)
  val trellis = Trellis.poly2trellis(constLen, codeGen)
  val trellisM = Refs.poly2trellisM(constLen, codeGen)

  type ViterbiAlgo = (Array[Int], Trellis[Int], (Int, Int) => Double) => Array[Int] // rxSymbols, trellis, metric => txSymbols

  def testHardDecision(algo: ViterbiAlgo, groupLength: Int) = {

    val inputData = ChainsawRand.nextBits(groupLength)
    val codedData = Refs.convenc(inputData.toArray, trellisM)
    val testCase = codedData.grouped(log2Up(trellis.numOutputSymbols))
      .map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
    val golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)

    val yours = algo(testCase, trellis, Algos.Hamming)
      .tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    assert(yours.length == testCase.length)
    println(s"input:  ${inputData.mkString("")}")
    println(s"coded:  ${codedData.mkString("")}")
    println(s"yours:  ${yours.mkString("")}")
    println(s"golden: ${golden.mkString("")}")
    assert(golden.mkString("") == yours.mkString(""))
  }

  behavior of "ViterbiAlgosTest"

  // FIXME: fix this after 11.15
  it should "viterbi" in {
    (0 until 10).map(_ => ChainsawRand.nextInt(200) + 10).foreach(testHardDecision(Algos.viterbi(_, _, _), _))
  }

  it should "viterbiTraceback" in {
    (0 until 10).map(_ => ChainsawRand.nextInt(200) + 10).foreach(testHardDecision(Algos.viterbiTraceback(_, _, _), _))
  }

  it should "viterbiTracebackMinplus" in {
    (0 until 10).map(_ => ChainsawRand.nextInt(200) + 10).foreach(testHardDecision(Algos.viterbiTracebackMinplus(_, _, _), _))
  }

  it should "viterbiParallel" in {
    (0 until 10).map(_ => (ChainsawRand.nextInt(50) + 2) * 4).foreach(testHardDecision(Algos.viterbiParallel(_, _, _, 0, 4), _))
  }

}

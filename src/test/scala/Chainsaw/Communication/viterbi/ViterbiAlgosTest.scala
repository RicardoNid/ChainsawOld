package Chainsaw.Communication.viterbi

import org.scalatest.flatspec.AnyFlatSpec

import Chainsaw._

class ViterbiAlgosTest extends AnyFlatSpec {

  val constLen = 7
  val codeGen = Array(177, 131)
  val trellis = Trellis.poly2trellis(constLen, codeGen)
  val trellisM = Refs.poly2trellisM(constLen, codeGen)

  val inputData = ((0 until 94).map(_ => DSPRand.nextInt(trellis.numInputSymbols)) ++ Seq.fill(constLen - 1)(0)).toArray
  val codedData = Refs.convenc(inputData, trellisM)
  val testCase = codedData.grouped(2).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
  val golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)

  def testAlgo(algo: (Array[Int], Trellis[Int], (Int, Int) => Double) => Array[Int]) = {
    val yours = algo(testCase, trellis, Algos.Hamming)
      .tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    assert(yours.length == testCase.length)
    assert(golden.mkString("") == yours.mkString(""))
  }

  behavior of "ViterbiAlgosTest"

  it should "viterbi" in {
    testAlgo(Algos.viterbi(_, _, _))
  }

  it should "viterbiTraceback" in {
    testAlgo(Algos.viterbiTraceback(_, _, _, 0))
  }

  it should "viterbiTracebackMinplus" in {
    testAlgo(Algos.viterbiTracebackMinplus(_, _, _, 0))
  }

  it should "viterbiParallel" in {
    testAlgo(Algos.viterbiParallel(_, _, _, 0, 4))
  }

}

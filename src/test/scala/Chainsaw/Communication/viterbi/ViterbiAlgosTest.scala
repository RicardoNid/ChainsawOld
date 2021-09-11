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

  behavior of "ViterbiAlgosTest"

  it should "viterbi" in {
    val yours = Algos.viterbi(testCase, trellis, Algos.Hamming).tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    assert(yours.length == testCase.length)
    assert(golden.mkString("") == yours.mkString(""))
  }

  it should "viterbiTraceback" in {
    val yours = Algos.viterbiTraceback(testCase, trellis, Algos.Hamming).tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    assert(yours.length == testCase.length)
    assert(golden.mkString("") == yours.mkString(""))
  }

  it should "viterbiParallel" in {
    val yours = Algos.viterbiParallel(testCase, trellis, Algos.Hamming, 0, 4)
      .tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)

    assert(yours.length == testCase.length)
    assert(golden.mkString("") == yours.mkString(""))
  }

}

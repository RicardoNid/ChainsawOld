package Chainsaw.comm.viterbi

import org.scalatest.flatspec.AnyFlatSpec

import Chainsaw._
import spinal.core._

class ViterbiAlgosTest extends AnyFlatSpec {

  // TODO: expand this to multi-in, multi-out

  val constLen = 7
  val codeGen = Array(177, 131)
  val trellis = Trellis.poly2trellis(constLen, codeGen)
  val trellisM = Refs.poly2trellisM(constLen, codeGen)

  val inputData = (0 until 100).map(_ => DSPRand.nextInt(trellis.numInputSymbols)).toArray
  val codedData = Refs.convenc(inputData, trellisM)
  val testCase = codedData.grouped(log2Up(trellis.numOutputSymbols)).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
  val golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)

  def testAlgo(algo: (Array[Int], Trellis[Int], (Int, Int) => Double) => Array[Int]) = {
    val yours = algo(testCase, trellis, Algos.Hamming)
      .tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    assert(yours.length == testCase.length)
    println(yours.mkString(""))
    println(golden.mkString(""))
    assert(golden.mkString("") == yours.mkString(""))
  }

  behavior of "ViterbiAlgosTest"

  // FIXME: fix this after 11.15
//  it should "viterbi" in {
//    testAlgo(Algos.viterbi(_, _, _))
//  }

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

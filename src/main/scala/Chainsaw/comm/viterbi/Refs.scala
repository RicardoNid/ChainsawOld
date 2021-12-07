package Chainsaw.comm.viterbi

import spinal.core._
import Chainsaw._
import Chainsaw.matlabIO._

object Refs {

  def poly2trellisM(constLen: Int, codeGen: Array[Int]) = {
    eng.feval[MStruct]("poly2trellis", Array(constLen.toDouble), codeGen.map(_.toDouble))
  }

  def vitdecHard(coded: Array[Int], trellis: MStruct, tblen: Int) =
    eng.feval[Array[Double]]("vitdec", coded.map(_.toDouble), trellis, Array(tblen.toDouble), "trunc", "hard").map(_.toInt)

  def convenc(data: Array[Int], trellis: MStruct) = eng.feval[Array[Int]]("convenc", data, trellis)

  /** Test data for 802.11n((2,1,7) coding with generator (177, 131))
   */
  def getTestData = {
    val constLen = 7
    val codeGen = Array(177, 131)
    val trellis = Trellis.poly2trellis(constLen, codeGen)
    val trellisM = Refs.poly2trellisM(constLen, codeGen)

    val inputData = (0 until 100).map(_ => ChainsawRand.nextInt(trellis.numInputSymbols)).toArray
    val codedData = Refs.convenc(inputData, trellisM)
    val testCases = codedData.grouped(log2Up(trellis.numOutputSymbols)).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
    val Golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)
    (trellis, testCases, Golden)
  }
}

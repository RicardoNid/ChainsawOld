package Chainsaw.Communication.viterbi

import Chainsaw.matlabIO._

object Refs {

  def poly2trellisM(constLen: Int, codeGen: Array[Int]) = {
    eng.feval[MStruct]("poly2trellis", Array(constLen.toDouble), codeGen.map(_.toDouble))
  }

  def vitdecHard(coded: Array[Int], trellis: MStruct, tblen: Int) =
    eng.feval[Array[Double]]("vitdec", coded.map(_.toDouble), trellis, Array(tblen.toDouble), "term", "hard").map(_.toInt)

  def convenc(data: Array[Int], trellis: MStruct) = eng.feval[Array[Int]]("convenc", data, trellis)
}

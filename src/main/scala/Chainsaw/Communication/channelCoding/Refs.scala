package Chainsaw.Communication.channelCoding

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._

object Refs {

  def poly2trellisM(constLen: Array[Int], codeGen: Array[Array[Int]]): MStruct = {
    eng.feval[MStruct]("poly2trellis", constLen.map(_.toDouble), codeGen.map(_.map(_.toDouble)))
  }

  def poly2trellisM(constLen: Int, codeGen: Array[Int]): MStruct = poly2trellisM(Array(constLen), Array(codeGen))

  def convenc(data: Array[Int], trellis: MStruct) = eng.feval[Array[Int]]("convenc", data, trellis)
}

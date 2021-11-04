package Chainsaw.comm.viterbi

import Chainsaw.matlabIO._

import scala.collection.mutable.{ArrayBuffer, Map}

case class Trellis[T](numInputSymbols: Int, numOutputSymbols: Int, numStates: Int, nextStates: Array[Array[Int]], outputs: Array[Array[T]]) {
  require(nextStates.length == numStates && nextStates.head.length == numInputSymbols)
  require(outputs.length == numStates && outputs.head.length == numInputSymbols)

  val lookBackMap = Map[Int, ArrayBuffer[(Int, T)]]() // lookBackMap(nextState) gets the sequence of (prevState, output)
  Seq.tabulate(numInputSymbols, numStates) { (inputSymbol: Int, prevState: Int) =>
    val nextState = nextStates(prevState)(inputSymbol)
    val outputSymbol = outputs(prevState)(inputSymbol)
    if (lookBackMap.isDefinedAt(nextState)) lookBackMap(nextState) += Tuple2(prevState, outputSymbol)
    else lookBackMap += nextState -> ArrayBuffer(Tuple2(prevState, outputSymbol))
  }
}

object Trellis {

  /** Generate trellis from config of convenc
   */
  def poly2trellis(constLen:Int, codeGen:Array[Int]) = {
    val matlabTrellis = Refs.poly2trellisM(constLen:Int, codeGen:Array[Int])
    Trellis(
      numInputSymbols = matlabTrellis.get("numInputSymbols").asInstanceOf[Double].toInt,
      numOutputSymbols = matlabTrellis.get("numOutputSymbols").asInstanceOf[Double].toInt,
      numStates = matlabTrellis.get("numStates").asInstanceOf[Double].toInt,
      nextStates = matlabTrellis.get("nextStates").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt)),
      outputs = matlabTrellis.get("outputs").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    )
  }

}



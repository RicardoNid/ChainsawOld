package Chainsaw.comm.viterbi

import scala.collection.mutable.{ArrayBuffer, Map}

/**
 * @param numInputSymbols  number of distinct input symbols
 * @param numOutputSymbols number of distinct output symbols
 * @param numStates        number of states
 * @param nextStates       2-D matrix of (state, next)
 * @param outputs
 * @see ''Matlab istrellis & poly2trellis''
 * @see for trellis''Coding Theory'' Chap3.2.2
 * @see we adopt the symbols (n,k,m) used in ''Coding Theory'' Chap3.1.2
 */
case class VitTrellis[T](numInputSymbols: Int, numOutputSymbols: Int, numStates: Int, nextStates: Array[Array[Int]], outputs: Array[Array[T]]) {

  require(nextStates.length == numStates && nextStates.head.length == numInputSymbols)
  require(outputs.length == numStates && outputs.head.length == numInputSymbols)

  // for convolution code
  //  val n = log2Up(numOutputSymbols)
  //  val k = log2Up

  val lookBackMap = Map[Int, ArrayBuffer[(Int, T)]]() // lookBackMap(nextState) gets the sequence of (prevState, output)
  Seq.tabulate(numInputSymbols, numStates) { (inputSymbol: Int, prevState: Int) =>
    val nextState = nextStates(prevState)(inputSymbol)
    val outputSymbol = outputs(prevState)(inputSymbol)
    if (lookBackMap.isDefinedAt(nextState)) lookBackMap(nextState) += Tuple2(prevState, outputSymbol)
    else lookBackMap += nextState -> ArrayBuffer(Tuple2(prevState, outputSymbol))
  }
}

object VitTrellis {

  /** Generate trellis from config of convenc
   */
  def poly2trellis(constLen: Int, codeGen: Array[Int]) = {
    val matlabTrellis = Refs.poly2trellisM(constLen: Int, codeGen: Array[Int])
    VitTrellis(
      numInputSymbols = matlabTrellis.get("numInputSymbols").asInstanceOf[Double].toInt,
      numOutputSymbols = matlabTrellis.get("numOutputSymbols").asInstanceOf[Double].toInt,
      numStates = matlabTrellis.get("numStates").asInstanceOf[Double].toInt,
      nextStates = matlabTrellis.get("nextStates").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt)),
      outputs = matlabTrellis.get("outputs").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    )
  }

}



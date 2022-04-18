package Chainsaw.algos

import Chainsaw.algos.MatlabRefs.poly2trellis
import Chainsaw.matlabIO._
import breeze.linalg._

case class StateTransition[T](prevState: Int, nextState: Int, input: Int, output: T)

/** a state machine(trellis) represented by integer symbols and states, this relate state machine to coding theory
 *
 * @param nextStateLogic lookup table of next state logic - A(currentState, inputSymbol) = nextState
 * @param outputLogic    lookup table of output logic A(currentState, inputSymbol) = outputSymbol
 * @tparam T data type of output symbols, could be int(for hard decision) or double(for soft decision)
 */
case class Trellis[T](nextStateLogic: DenseMatrix[Int], outputLogic: DenseMatrix[T]) {

  // check validity
  require(nextStateLogic.rows == outputLogic.rows)
  require(nextStateLogic.cols == outputLogic.cols)

  // attributes
  val numStates = nextStateLogic.rows
  val numInputSymbols = nextStateLogic.cols
  val numOutputSymbols = outputLogic.toArray.distinct.size
  val numTransitions = numStates * numInputSymbols

  // pre-built table, A(nextState) = transition
  val transitionsTo: Seq[Seq[StateTransition[T]]] = (0 until numStates).map { state =>
    val indices = nextStateLogic.findAll(_ == state)
    indices.map(tuple => StateTransition(tuple._1, state, tuple._2, outputLogic(tuple._1, tuple._2)))
  }

  // pre-built table, A(prevState, nextState) = transition
  val transitionsMap: Map[(Int, Int), StateTransition[T]] = transitionsTo.flatten.map(
    transition => (transition.prevState, transition.nextState) -> transition).toMap

  val transitions = transitionsMap.values.toSeq

  // methods
  def getPrevStatesTo(state: Int) = nextStateLogic.findAll(_ == state).map(_._1)

  def getTransitionsTo(state: Int) = transitionsTo(state)

  def getTransition(prevState: Int, nextState: Int) = transitionsMap(prevState, nextState)
}

object Trellis {

  /** create a coding state machine from a matlab trellis struct
   */
  def fromMatlab(trellis: MStruct) = {
    val nextStates = trellis.get("nextStates").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    val outputs = trellis.get("outputs").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    Trellis(
      nextStateLogic = new DenseMatrix[Int](nextStates.head.length, nextStates.length, nextStates.flatten).t,
      outputLogic = new DenseMatrix[Int](outputs.head.length, outputs.length, outputs.flatten).t
    )
  }

  // TODO: this is for 1/n code rate, k/n code rate should be implemented, for that, polys would be a 2-D array

  /** create a coding state machine from a generator polynomials, we adopt the definition of constraint length in ''Coding Theory'', so +1 for matlab
   *
   * @param constLen number of memory
   *
   */
  def fromPoly(constLen: Int, polys: Array[Int]) = fromMatlab(poly2trellis(constLen + 1, polys))

  def main(args: Array[String]): Unit = {
    val trellis = poly2trellis(6, Array(171, 133))
    val stateMachine = fromMatlab(trellis)
  }
}
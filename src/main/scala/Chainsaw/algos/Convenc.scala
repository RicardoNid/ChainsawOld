package Chainsaw.algos

import Chainsaw._
import breeze.linalg._

object Convenc {

  /** plain implementation of convenc by looking up next state/output logic table
   * @see ''Coding Theory'' Chap3.1.4 for termination mode, 3.2.3 for Viterbi algorithm
   */
  @definition
  def convenc(txSymbols: DenseVector[Int], trellis: Trellis[Int]) = {
    val initState = 0
    val states = txSymbols.toArray.scan(initState)((state, tx) => trellis.nextStateLogic(state, tx))
    DenseVector(states.init.zip(txSymbols.toArray).map{ case (state, tx) => trellis.outputLogic(state, tx)})
  }


//  /** implement convenc as matrix multiplication in finite field F2
//   */
//  def convencByMatrix(txSymbols: DenseVector[Int], trellis: Trellis[Int]):DenseVector[Int] ={
//
//  }
}

package Chainsaw.dsl.transform

import Chainsaw.dsl._

class System[TIn, TOut](algo: Algo[TIn, TOut], impls: Seq[Impl], repetitions: Seq[Repetition]) {

  def apply(dataIn: Array[TIn]) = algo(dataIn)

  def composite[TPrev](that: Transform[TPrev, TIn]) = {
    val newAlgo = (dataIn: Array[TPrev]) => this.apply(that.apply(dataIn))
    val newImpls = impls :+ that.base.impl
    val newRepetitions = repetitions :+ that.repetition
    new System[TPrev, TOut](newAlgo, newImpls, newRepetitions)
  }

  def °[TPrev](that: Transform[TPrev, TIn]) = composite(that)

  def build() = {



  }

}

object System {
  def apply[TIn, TOut](algo: Algo[TIn, TOut], impls: Seq[Impl], repetitions: Seq[Repetition]): System[TIn, TOut] = new System(algo, impls, repetitions)
}

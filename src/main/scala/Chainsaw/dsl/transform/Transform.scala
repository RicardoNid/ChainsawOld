package Chainsaw.dsl.transform

import scala.reflect.ClassTag

class Transform[TIn, TOut](val transform: Array[TIn] => Array[TOut], val parallel: Int, val iterative: Int)
                          (implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut]) {

  def forceTransform(dataIn: Array[TIn]) = transform(dataIn).asInstanceOf[Array[TIn]]

  def apply(dataIn: Array[TIn]) = dataIn.sliding(dataIn.length / parallel)
    .map(data => Array.iterate(data, iterative + 1)(forceTransform).last)
    .toArray.flatten.asInstanceOf[Array[TOut]]

  def composite[TPrev](that: Transform[TPrev, TIn])(implicit tag: ClassTag[TPrev]) = {
    val current = this
    val newTransform = (dataIn: Array[TPrev]) => current.apply(that.apply(dataIn))
    new Transform[TPrev, TOut](newTransform, 1, 1)
  }

  // composition operator
  def *[TPrev](that: Transform[TPrev, TIn])(implicit tag: ClassTag[TPrev]) = composite(that)

  def ⊗(parallel: Int) = new Transform(transform, this.parallel * parallel, iterative)

  def ^(iterative: Int) = new Transform(transform, this.parallel, this.iterative * iterative)
}
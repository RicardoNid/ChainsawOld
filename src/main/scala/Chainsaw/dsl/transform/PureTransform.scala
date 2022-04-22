package Chainsaw.dsl.transform

import Chainsaw.dsl.field.Field

import scala.reflect.ClassTag

class PureTransform[TIn, TOut]
(val base: BaseTransform[TIn, TOut], val repetition: Repetition)
(implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut], fieldIn: Field[TIn], fieldOut: Field[TOut]) {

  def ⊗(parallel: Int, step: Int = -1) =
    new PureTransform(base, repetition.⊗(parallel, step))

  def ^(iterative: Int) =
    new PureTransform(base, repetition.^(iterative))

  def size = repetition.expand(base.size)

  def inputSize = size._1

  def outputSize = size._2

  def apply(dataIn: Array[TIn]) = {
    require(dataIn.length == inputSize, s"input size should be $inputSize")
    var segments = Seq(dataIn)

    // get segments according to space reuse
    repetition.space.reverse.foreach { rep =>
      segments =
        if (rep.step == -1) segments.map(segment => segment.grouped(segment.length / rep.group).toArray).flatten
        else segments.map(segment => segment.sliding(segment.length - rep.step * (rep.group - 1), rep.step).toArray).flatten
    }

    def forceTransform(dataIn: Array[TIn]) = base.algo(dataIn).asInstanceOf[Array[TIn]]

    // iterate according to time reuse
    val ret = segments.map(data =>
      if (repetition.time.group == 1) base.algo(data)
      else Array.iterate(data, repetition.time.group + 1)(forceTransform).last.asInstanceOf[Array[TOut]]
    ).flatten.toArray

    require(ret.length == outputSize)
    ret
  }

  def toSystem = System(apply, Seq(base.impl), Seq(repetition))

  def composite[TPrev](that: PureTransform[TPrev, TIn]) = toSystem.composite(that)
}

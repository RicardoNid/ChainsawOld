package Chainsaw.dsl

import scala.reflect.ClassTag

class BaseTransform[TIn, TOut]
(val algo: Algo[TIn, TOut],
 val impl: Impl)
(implicit val typeIn: MixType[TIn], val typeOut: MixType[TOut]) {

  implicit val tagIn: ClassTag[TIn] = typeIn.tag
  implicit val tagOut: ClassTag[TOut] = typeOut.tag

  def ⊗(parallel: Int, step: Int = -1) =
    new Transform(this, Repetition(Seq(SpaceRepetition(parallel, step)), TimeRepetition(1)))

  def ^(iterative: Int) =
    new Transform(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(iterative)))

  def toPure: Transform[TIn, TOut] = new Transform(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(1)))

}


object BaseTransform {
  def apply[TIn: ClassTag, TOut: ClassTag]
  (transform: Algo[TIn, TOut], impl: Impl)
  (implicit fieldIn: MixType[TIn], fieldOut: MixType[TOut]): BaseTransform[TIn, TOut] =
    new BaseTransform(transform, impl)
}



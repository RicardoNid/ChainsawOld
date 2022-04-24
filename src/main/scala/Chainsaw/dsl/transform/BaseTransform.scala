package Chainsaw.dsl.transform

import Chainsaw.dsl.field.MixType
import Chainsaw.dsl._
import spinal.core._

import scala.reflect.{ClassTag}

class BaseTransform[TIn, TOut]
(val algo: Algo[TIn, TOut],
 val impl: Impl,
 val size: (Int, Int))
(implicit fieldIn: MixType[TIn], fieldOut: MixType[TOut]) {

  implicit val tagIn: ClassTag[TIn] = fieldIn.tag
  implicit val tagOut: ClassTag[TOut] = fieldOut.tag

  def ⊗(parallel: Int, step: Int = -1) =
    new Transform(this, Repetition(Seq(SpaceRepetition(parallel, step)), TimeRepetition(1)))

  def ^(iterative: Int) =
    new Transform(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(iterative)))

  def toPure: Transform[TIn, TOut] = new Transform(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(1)))

}


object BaseTransform {
  def apply[TIn:ClassTag, TOut:ClassTag](transform: Array[TIn] => Array[TOut], impl: Vec[Bits] => Vec[Bits], size: (Int, Int))
                      (implicit fieldIn: MixType[TIn], fieldOut: MixType[TOut]): BaseTransform[TIn, TOut] =
    new BaseTransform(transform, impl, size)
}



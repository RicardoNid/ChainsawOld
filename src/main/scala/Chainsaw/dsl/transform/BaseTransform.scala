package Chainsaw.dsl.transform

import Chainsaw.dsl.field.Field
import Chainsaw.dsl._
import spinal.core._

import scala.reflect.{ClassTag}

class BaseTransform[TIn, TOut]
(val algo: Algo[TIn, TOut],
 val impl: Impl,
 val size: (Int, Int))
(implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut], fieldIn: Field[TIn], fieldOut: Field[TOut]) {


  def ⊗(parallel: Int, step: Int = -1) =
    new PureTransform(this, Repetition(Seq(SpaceRepetition(parallel, step)), TimeRepetition(1)))

  def ^(iterative: Int) =
    new PureTransform(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(iterative)))

  def toPure: PureTransform[TIn, TOut] = new PureTransform(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(1)))

}


object BaseTransform {
  def apply[TIn, TOut](transform: Array[TIn] => Array[TOut], impl: Vec[Bits] => Vec[Bits], size: (Int, Int))
                      (implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut], fieldIn: Field[TIn], fieldOut: Field[TOut]): BaseTransform[TIn, TOut] =
    new BaseTransform(transform, impl, size)(tagIn, tagOut, fieldIn, fieldOut)
}



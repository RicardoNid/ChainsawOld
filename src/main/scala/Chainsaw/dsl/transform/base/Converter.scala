package Chainsaw.dsl.transform.base

import Chainsaw.dsl.{BaseTransform, _}
import Chainsaw.dsl.transform.base.Converter._
import spinal.core._

import scala.language.postfixOps
import scala.reflect.{ClassTag, classTag}

class Converter[TIn: ClassTag, TOut: ClassTag](sizeIn: Int, sizeOut: Int, fieldIn: MixType[TIn], fieldOut: MixType[TOut])
  extends BaseTransform[TIn, TOut](
    getAlgo(fieldIn, fieldOut),
    getImpl(fieldOut),
    (sizeIn, sizeOut)
  )(fieldIn, fieldOut)

object Converter {

  def getAlgo[TIn, TOut: ClassTag](fieldIn: MixType[TIn], fieldOut: MixType[TOut]): Algo[TIn, TOut] =
    (dataIn: Array[TIn]) => {
      dataIn
        .map(fieldIn.toBits)
        .reduce(_ ++ _)
        .grouped(fieldOut.width).toArray
        .map(fieldOut.fromBits)
    }

  // TODO: IMPLEMENT
  def getImpl[TIn, TOut](fieldOut: MixType[TOut]) =
    (dataIn: Vec[Bits]) => dataIn.asBits.subdivideIn(fieldOut.width bits)

  def apply[TIn: ClassTag, TOut: ClassTag]
  (sizeIn: Int, sizeOut: Int, fieldIn: MixType[TIn], fieldOut: MixType[TOut]): Converter[TIn, TOut] =
    new Converter(sizeIn, sizeOut, fieldIn, fieldOut)

}

package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import Converter._
import spinal.core._

import scala.language.postfixOps
import scala.reflect.{ClassTag, classTag}

class Converter[TIn: ClassTag, TOut: ClassTag](sizeIn: Int, sizeOut: Int, fieldIn: MixType[TIn], fieldOut: MixType[TOut])
  extends dsl.BaseTransform[TIn, TOut](
    getAlgo(fieldIn, fieldOut),
    getImpl(sizeIn, sizeOut, fieldOut)
  )(fieldIn, fieldOut)

object Converter {

  def getAlgo[TIn, TOut: ClassTag](typeIn: MixType[TIn], typeOut: MixType[TOut]): Algo[TIn, TOut] =
    (dataIn: Array[TIn]) => {
      dataIn
        .map(typeIn.toBits)
        .reduce(_ ++ _)
        .grouped(typeOut.width).toArray
        .map(typeOut.fromBits)
    }

  // TODO: IMPLEMENT
  def getImpl[TIn, TOut](sizeIn: Int, sizeOut: Int, fieldOut: MixType[TOut]) = new Impl(
    (sizeIn, sizeOut),
    (dataIn: Vec[Bits]) => {
      dataIn.setName("converterIn")
      val ret = Vec(dataIn.reverse).asBits.subdivideIn(fieldOut.width bits)
      ret.setName("converterOut")
    },
    0
  )

  def apply[TIn: ClassTag, TOut: ClassTag]
  (sizeIn: Int, sizeOut: Int, fieldIn: MixType[TIn], fieldOut: MixType[TOut]): Converter[TIn, TOut] =
    new Converter(sizeIn, sizeOut, fieldIn, fieldOut)

}

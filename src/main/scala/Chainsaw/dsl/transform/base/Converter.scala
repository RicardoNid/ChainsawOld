package Chainsaw.dsl.transform.base

import Chainsaw.dsl.{BaseTransform, _}
import Chainsaw.dsl.transform.base.Converter._
import spinal.core._

import scala.reflect.ClassTag

class Converter[TIn, TOut](sizeIn: Int, sizeOut: Int, fieldIn: Field[TIn], fieldOut: Field[TOut])
                          (implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut])
  extends BaseTransform[TIn, TOut](
    getAlgo(fieldIn, fieldOut),
    getImpl(fieldOut),
    (sizeIn, sizeOut)
  )(tagIn, tagOut, fieldIn, fieldOut)

object Converter {

  def getAlgo[TIn, TOut: ClassTag](fieldIn: Field[TIn], fieldOut: Field[TOut]): Algo[TIn, TOut] =
    (dataIn: Array[TIn]) => {
      dataIn
        .map(fieldIn.toBits)
        .reduce(_ ++ _)
        .grouped(fieldOut.width).toArray
        .map(fieldOut.fromBits)
    }

  // TODO: IMPLEMENT
  def getImpl[TIn, TOut](fieldOut: Field[TOut]) =
    (dataIn: Vec[Bits]) => dataIn.asBits.subdivideIn(fieldOut.width bits)

}

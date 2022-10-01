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
    new ConverterImpl(sizeIn, sizeOut, fieldOut.width)
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

  def apply[TIn: ClassTag, TOut: ClassTag]
  (sizeIn: Int, sizeOut: Int, fieldIn: MixType[TIn], fieldOut: MixType[TOut]): Converter[TIn, TOut] =
    new Converter(sizeIn, sizeOut, fieldIn, fieldOut)

}

class ConverterImpl(sizeIn: Int, sizeOut: Int, widthOut: Int) extends Impl {
  override val name = "Converter"
  override val width = (widthOut * sizeOut / sizeIn, widthOut)
  override val size = (sizeIn, sizeOut)
  override def getLatency(fold: Int) = 0
  override def getFunction(fold: Int) = (dataIn: Vec[Bits]) => Vec(dataIn.reverse).asBits.subdivideIn(widthOut bits)

}
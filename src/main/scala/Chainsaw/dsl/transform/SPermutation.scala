package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import spinal.core._

import scala.reflect.ClassTag


class SPermutation[T](val row: Int, val column: Int)
                     (implicit tag: ClassTag[T], field: MixType[T])
  extends dsl.BaseTransform[T, T](
    SPermutation.getTransform(row, column),
    SPermutation.getImpl(row, column))(field, field)


object SPermutation {

  def getTransform[T: ClassTag](row: Int, column: Int): Algo[T, T] =
    (dataIn: Array[T]) => {
      require(dataIn.length == row * column)
      dataIn.grouped(row).toArray.transpose.flatten
    }

  def getImpl(row: Int, column: Int) = new Impl(
    (row * column, row * column),
    (dataIn: Vec[Bits]) => Vec(dataIn.toArray.grouped(row).toArray.transpose.flatten),
    latency = 0
  )

  def getSize(row: Int, column: Int) = (row * column, row * column)

  def apply[T](row: Int, column: Int)
              (implicit tag: ClassTag[T], field: MixType[T]): SPermutation[T] =
    new SPermutation(row, column)(tag, field)

}

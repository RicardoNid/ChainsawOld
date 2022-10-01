package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import spinal.core._

import scala.reflect.ClassTag


class SPerm[T](val row: Int, val column: Int)
              (implicit tag: ClassTag[T], field: MixType[T])
  extends dsl.BaseTransform[T, T](SPerm.getTransform(row, column), new SPermImpl(row, column, field.width))(field, field)

object SPerm {

  def getTransform[T: ClassTag](row: Int, column: Int): Algo[T, T] =
    (dataIn: Array[T]) => {
      require(dataIn.length == row * column)
      dataIn.grouped(row).toArray.transpose.flatten
    }

  def apply[T](row: Int, column: Int)
              (implicit tag: ClassTag[T], field: MixType[T]): SPerm[T] =
    new SPerm(row, column)(tag, field)

}

class SPermImpl(row: Int, column: Int, theWidth: Int) extends Impl {
  override val name = "SPerm"
  override val foldMax = row * column
  override val width = (theWidth, theWidth)
  override val size = (row * column, row * column)
  override def getFunction(fold: Int) = (dataIn: Vec[Bits]) =>
  Vec(dataIn.toArray.grouped(row).toArray.transpose.flatten)
  override def getLatency(fold: Int) = 0

}

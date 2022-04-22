package Chainsaw.dsl.transform.base

import Chainsaw.dsl._
import spinal.core._
import scala.reflect.ClassTag

import LUT._

class LUT[T](val lut: Array[T])
            (implicit classTag: ClassTag[T], fieldIn: Field[Int], fieldOut: Field[T])
  extends BaseTransform[Int, T](
    getAlgo(lut),
    getImpl(lut),
    (1,1))

object LUT {

  def getAlgo[T](lut: Array[T])(implicit classTag: ClassTag[T]) =
    (dataIn: Array[Int]) => {
      require(dataIn.length == 1)
      Array(lut(dataIn.head))
    }

  def getImpl[T](lut: Array[T])(implicit classTag: ClassTag[T], field: Field[T]) =
    (dataIn: Vec[Bits]) => {
      val rom = Mem(lut.map(field.toHard))
      Vec(rom.readSync(dataIn.head.asUInt))
    }

  def apply[T](lut: T*)
              (implicit classTag: ClassTag[T], fieldIn: Field[Int], fieldOut: Field[T]): LUT[T] =
    new LUT(lut.toArray)
}

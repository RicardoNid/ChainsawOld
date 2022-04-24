package Chainsaw.dsl.transform.base

import Chainsaw.dsl._
import spinal.core._
import scala.reflect.ClassTag

import LUT._

class LUT[T: ClassTag](val lut: Array[T])
                      (implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T])
  extends BaseTransform[Int, T](
    getAlgo(lut),
    getImpl(lut),
    (1, 1))

object LUT {

  def getAlgo[T: ClassTag](lut: Array[T]) =
    (dataIn: Array[Int]) => {
      require(dataIn.length == 1)
      Array(lut(dataIn.head))
    }

  def getImpl[T: ClassTag](lut: Array[T])(implicit mixType: MixType[T]) =
    (dataIn: Vec[Bits]) => {
      val rom = Mem(lut.map(mixType.toCoeff))
      Vec(rom.readSync(dataIn.head.asUInt))
    }

  def apply[T: ClassTag](lut: T*)
                        (implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T]): LUT[T] =
    new LUT(lut.toArray)
}

package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import spinal.core._

import scala.reflect.ClassTag
import LUT._

class LUT[T: ClassTag]
(val lut: Array[T])
(implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T])
  extends dsl.BaseTransform[Int, T](
    getAlgo(lut),
    new LutImpl(lut.map(mixTypeOut.toBits)))

object LUT {

  def getAlgo[T: ClassTag](lut: Array[T]): Algo[Int, T] =
    (dataIn: Array[Int]) => {
      require(dataIn.length == 1)
      Array(lut(dataIn.head))
    }

  def apply[T: ClassTag]
  (lut: T*)
  (implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T]): LUT[T] =
    new LUT(lut.toArray)
}
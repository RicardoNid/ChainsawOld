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
    getImpl(lut))

object LUT {

  def getAlgo[T: ClassTag](lut: Array[T]): Algo[Int, T] =
    (dataIn: Array[Int]) => {
      require(dataIn.length == 1)
      Array(lut(dataIn.head))
    }

  def getImpl[T: ClassTag](lut: Array[T])(implicit mixType: MixType[T]): Impl =
    new Impl(
      size = (1, 1),
      impl = (dataIn: Vec[Bits]) => {
        val rom = Mem(lut.map(mixType.toCoeff))
        Vec(rom.readAsync(dataIn.head.asUInt))
      },
      latency = 0
    )

  def apply[T: ClassTag]
  (lut: T*)
  (implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T]): LUT[T] =
    new LUT(lut.toArray)
}

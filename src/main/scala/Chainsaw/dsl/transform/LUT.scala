package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import Chainsaw.dsl.transform.LUT._
import spinal.core._

import scala.reflect.ClassTag

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

class LutImpl(lut: Array[String]) extends Impl {
  override val name = "LUT"
  override val width = (log2Up(lut.length), lut.head.length)
  override def getLatency(fold: Int) = 1
  override def getFunction(fold: Int) = (dataIn: Vec[Bits]) => {
    val ROM = Mem(lut.map(B(_)))
    Vec(ROM.readSync(dataIn.head.asUInt))
  }
}
package Chainsaw.dsl.transform

import Chainsaw.dsl._
import Diagonal._
import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

class Diagonal[T:ClassTag](coeffs:Array[T])(implicit ring: Ring[T])
  extends BaseTransform [T,T](getAlgo(coeffs), new DiagonalImpl(coeffs.map(ring.toBits), ring.multH))

object Diagonal {

  def apply[T: ClassTag](coeffs: Array[T])(implicit ring: Ring[T]): Diagonal[T] = new Diagonal(coeffs)

  def getAlgo[T: ClassTag](coeffs: Array[T])(implicit ring: Ring[T]) =
  (dataIn: Array[T]) => {coeffs.zip(dataIn).map { case (coeff, data) => ring.mult(coeff, data) }}

}

class DiagonalImpl(coeffs: Array[String], multH: HardOp2) extends Impl {

  override val foldMax = coeffs.length

  override val size = (coeffs.length, coeffs.length)

  override def getImpl(fold:Int) = {
    val latency = multH.latency
    val impl = (dataIn: (Vec[Bits], Bool)) => {

      val coeffsHard = coeffs.map(B(_))
      val ret = Vec({coeffsHard .zip(dataIn._1).map { case (coeff, data) => multH.op(coeff, data) }})
      (ret, Delay(dataIn._2, latency, init = False))
    }
    RawImpl(impl, latency)
  }
}
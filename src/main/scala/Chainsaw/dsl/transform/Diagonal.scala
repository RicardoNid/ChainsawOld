package Chainsaw.dsl.transform

import Chainsaw.dsl._
import Diagonal._
import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

class Diagonal[T: ClassTag](coeffs: Array[T])(implicit ring: Ring[T])
  extends BaseTransform[T, T](getAlgo(coeffs), new DiagonalImpl(coeffs.map(ring.toBits), ring.multH))

object Diagonal {

  def apply[T: ClassTag](coeffs: Array[T])(implicit ring: Ring[T]): Diagonal[T] = new Diagonal(coeffs)

  def getAlgo[T: ClassTag](coeffs: Array[T])(implicit ring: Ring[T]) =
    (dataIn: Array[T]) => {
      coeffs.zip(dataIn).map { case (coeff, data) => ring.mult(coeff, data) }
    }

}

class DiagonalImpl(coeffs: Array[String], multH: HardOp2) extends Impl {
  override val name = "Diagonal"
  override val foldMax = coeffs.length
  override val width = (coeffs.head.length, coeffs.head.length)
  override val size = (coeffs.length, coeffs.length)

  override def getLatency(fold: Int) = fold * multH.latency

  override def getFunction(fold: Int) = (dataIn: Vec[Bits]) => {
    val coeffsHard = coeffs.map(B(_))
    Vec(coeffsHard.zip(dataIn).map { case (coeff, data) => multH.op(coeff, data) })
  }
}
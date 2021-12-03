package Chainsaw.core

import scala.collection.immutable
import scala.math.{Pi, cos}

object BasicTransforms {

  def sop(A: Seq[Double], B: Seq[Double]) = A.zip(B).map { case (a, b) => a * b }.sum

  def dct(data: Seq[Double]): immutable.Seq[Double] = {
    val N = data.length
    val index = (i: Int, k: Int) => (2 * i + 1) * k * Pi / (2 * N)
    (0 until N).map { k =>
      val coeffs = (0 until N).map(i => cos(index(i, k)))
      sop(data, coeffs)
    }
  }
}

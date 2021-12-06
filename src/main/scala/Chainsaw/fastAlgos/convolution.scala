package Chainsaw.fastAlgos

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

object convolution {

  def sop(a: DenseVector[Complex], b: DenseVector[Complex]): Complex = sum(a *:* b)

  def conv1d(data: DenseVector[Complex], coeff: DenseVector[Complex]) = {

  }

  def cyclicConv1d(data: DenseVector[Complex], coeff: DenseVector[Complex]) = {
    val N = data.length
    require(coeff.size == N)
    DenseVector.tabulate(N)(i => sop(data.rotateRight(i), coeff.reverse))
  }

  def main(args: Array[String]): Unit = {
//    println(cyclicConv1d(DenseVector(1 + i, 2, 3, 4), DenseVector(2 + i, 3, 4, 5)))
  }

}

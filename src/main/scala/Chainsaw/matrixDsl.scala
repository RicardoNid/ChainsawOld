package Chainsaw

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import breeze.storage.Zero

import scala.reflect.ClassTag

object matrixDsl {

  /** construct a permutation matrix
   *
   * @param permutation defining the permutation by an ordered sequence of original elements
   * @example Seq(0,2,1) place the third element in the second place and the second element in the third place
   */
  def P[T](permutation: Array[Int])(implicit ring: Semiring[T], tag: ClassTag[T]): DenseMatrix[T] = {
    val n = permutation.length
    val ret = DenseMatrix.zeros[T](n, n)
    permutation.indices.foreach(i => ret(permutation(i), i) = implicitly[Semiring[T]].one)
    ret
  }

  def Pad[T](padding: Array[Int])(implicit ring: Semiring[T], tag: ClassTag[T]): DenseMatrix[T] = {
    val n = padding.sum // size of input vector
    val m = padding.length // size of output vector
    val ret = DenseMatrix.zeros[T](m, n)
    var index = 0
    padding.indices.foreach { i =>
      if (padding(i) == 1) {
        ret(i, index) = implicitly[Semiring[T]].one
        index += 1
      }
    }
    ret
  }

  def Filter[T](filtering: Array[Int])(implicit ring: Semiring[T], tag: ClassTag[T]): DenseMatrix[T] = {
    val n = filtering.length // size of input vector
    val m = filtering.sum // size of output vector
    val ret = DenseMatrix.zeros[T](m, n)
    var index = 0
    filtering.indices.foreach { i =>
      if (filtering(i) == 1) {
        ret(index, i) = implicitly[Semiring[T]].one
        index += 1
      }
    }
    ret
  }

  /** expand a transform by sliding method
   *
   * @param transform original transform represented by a matrix
   * @param step      sliding step on input vector
   * @param k         expansion factor
   */
  def Slide[T](transform: DenseMatrix[T], step: Int, k: Int)(implicit ring: Semiring[T], tag: ClassTag[T]): DenseMatrix[T] = {
    val n = transform.cols + (k - 1) * step // size of input vector
    val m = transform.rows * k // size of output vector
    val ret = DenseMatrix.zeros[T](m, n)
    (0 until k).foreach { i =>
      val rowBegin = i * transform.rows
      val colBegin = i * step
      Array.tabulate(transform.rows, transform.cols) { (i, j) =>
        ret(rowBegin + i, colBegin + j) = transform(i, j)
      }
    }
    ret
  }

  /** expand a transform k times by Kronecker product(tensor product) with identity matrix
   *
   */
  def Tensor[T](transform: DenseMatrix[T], k: Int)(implicit ring: Semiring[T], tag: ClassTag[T]): DenseMatrix[T] =
    Slide(transform, k, k)

  def main(args: Array[String]): Unit = {

    // defining the ring we work on
    implicit val ring = implicitly[Semiring[Int]]

    // test on permutation matrix
    val permutation = Array(0, 2, 1, 3)
    val permuted: DenseVector[Int] = P(permutation) * DenseVector(0, 1, 2, 3)
    assert(permuted == new DenseVector(permutation))

    val padding = Array(1, 0, 1, 0, 1)
    val padded = Pad(padding) * DenseVector(3, 3, 3)
    assert(padded == DenseVector(3, 0, 3, 0, 3))

    val filtering = Array(1, 0, 1, 0, 1)
    val filtered = Filter(filtering) * DenseVector(1, 2, 3, 4, 5)
    assert(filtered == DenseVector(1, 3, 5))

    val transform = new DenseMatrix(3, 2, Array(1, 2, 3, 4, 5, 6))
    println(transform)
    println(Slide(transform, 1, 3))
    val input = DenseVector(1, 2, 3, 4)
    assert(
      Slide(transform, 1, 3) * input ==
        DenseVector(input.toArray.sliding(2, 1).toArray.map(transform * new DenseVector(_)).flatMap(_.toArray))
    )

  }

}

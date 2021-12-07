package Chainsaw.DSP.dct

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._


object Algo {

  /** type2 DCT
   *
   * @see [[https://en.wikipedia.org/wiki/Discrete_cosine_transform#DCT-II]]
   */
  def dct1D(data: DenseVector[Double]): DenseVector[Double] = {
    val N = data.length
    val coeffs = DenseMatrix.tabulate(N, N) { (k, i) =>
      val index = (2 * i + 1) * k * Pi / (2 * N)
      cos(index)
    }
    coeffs * data
  }

  /** inverse of type2 DCT
   *
   * @see [[https://en.wikipedia.org/wiki/Discrete_cosine_transform#Inverse_transforms]]
   */
  def idct1D(data: DenseVector[Double]): DenseVector[Double] = {
    val N = data.length
    val coeffs = DenseMatrix.tabulate(N, N) { (i, k) =>
      val delta = if (k == 0) 0.5 else 1
      val index = (2 * i + 1) * k * Pi / (2 * N)
      delta * cos(index)
    }
    (coeffs * data) /:/ (N.toDouble / 2)
  }

  /**
   * @see [[#Fast Algorithms for Signal Processing, Theorem 3.5.1, Corollary 3.5.2]]
   */
  def dct1DByDft(data: DenseVector[Double], method: Int): DenseVector[Double] = {
    val N = data.length
    val array = data.toArray

    val reverse = data(-1 to 0 by -1)
    val seq2n = DenseVector.vertcat(data, reverse) /:/ 2.0
    val zeros = DenseVector.zeros[Double](2 * N)
    val seq4n = DenseMatrix.vertcat(zeros.toDenseMatrix, seq2n.toDenseMatrix).reshape(4 * N, 1).toDenseVector

    if (method == 0) { // Theorem 3.5.1
      fourierTr.dvDouble1DFFT(seq4n)(0 until N).map(_.real)
    } else { // Corollary 3.5.2
      val factors = DenseVector.tabulate(N)(k => exp(-i * (2 * Pi * k / (4 * N))))
      val dft = fourierTr.dvDouble1DFFT(seq2n)(0 until N)
      (factors *:* dft).map(_.real)
    }
  }

  def main(args: Array[String]): Unit = {
    val data = DenseVector[Double](1, 2, 3, 4)

    val dct0 = dct1D(data)
    val dct1 = dct1DByDft(data,0)
    val dct2 = dct1DByDft(data,1)

    val diff0 = dct0 - dct1
    assert(abs(diff0).forall(_ < 0.001))
    val diff1 = dct0 - dct2
    assert(abs(diff1).forall(_ < 0.001))
  }

}



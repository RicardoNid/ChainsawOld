package Chainsaw.algos

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.i
import breeze.numerics.{cos, exp}
import breeze.numerics.constants.Pi
import breeze.signal.fourierTr

/** definition and fast algorithms of DCT, which is closely related to DFT
 *
 */
object Dct {

  /** type2 DCT
   *
   * @see [[https://en.wikipedia.org/wiki/Discrete_cosine_transform#DCT-II]]
   */
  @definition
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
  @definition
  def idct1D(data: DenseVector[Double]): DenseVector[Double] = {
    val N = data.length
    val coeffs = DenseMatrix.tabulate(N, N) { (i, k) =>
      val delta = if (k == 0) 0.5 else 1
      val index = (2 * i + 1) * k * Pi / (2 * N)
      delta * cos(index)
    }
    (coeffs * data) /:/ (N.toDouble / 2)
  }

  /** implement dct1D by dft
   * @see ''Fast Algorithms for Signal Processing'', Theorem 3.5.1, Corollary 3.5.2
   */
  @fastAlgo
  def dct1DByDft(data: DenseVector[Double], method: Int): DenseVector[Double] = {
    val N = data.length

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

}

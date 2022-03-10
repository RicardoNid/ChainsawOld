package Chainsaw.algos

import Chainsaw._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.i
import breeze.numerics.constants.Pi
import breeze.numerics.{cos, exp, pow}
import breeze.signal.fourierTr
import spinal.core._

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
   *
   * @see ''Fast Algorithms for Signal Processing'', Theorem 3.5.1, Corollary 3.5.2
   */
  @fastAlgo("dct1D")
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

  /** fast algorithm for dct by recursive matrix decomposition
   *
   * @see ''A fast computational algorithm for the discrete cosine transform, IEEE Transactions on Communications'' for method 0
   * @see ''A  new  algorithm  to  compute  the  discrete  cosine  transform,  IEEE Transactions on Acoustics, Speech and Signal Processing'' for method 1
   */
  @fastAlgo("dct1D")
  def dct1DByMatrix(data: DenseVector[Double], method: Int = 0) = {

    implicit class SeqUtil(data: Seq[Double]) {
      // transformation on a column vector, i.e. T * v
      def t(transform: Seq[Double] => Seq[Double]) = transform(data)

      // two transformations on upper/lower half of a column vector, i.e. T_0 * v_0 ++ T_1 * v_1
      def t2(tarns0: Seq[Double] => Seq[Double], tarns1: Seq[Double] => Seq[Double]) = {
        val n = data.length
        tarns0(data.take(n / 2)) ++ tarns1(data.takeRight(n / 2))
      }
    }

    val mode = 0

    def build(data: Seq[Double]): Seq[Double] = {
      val n = data.length
      val (add, mult) = if (mode == 0) (T _, M _) // method proposed by reference 0, 1977
      else (A _, R _) // method proposed by reference 1, 1984

      if (n == 2) Seq(data(0) + data(1), cos(Pi / 4) * (data(0) - data(1)))
      else data.t(II).t2(I, mult).t2(build, build).t2(I, add).t(S)
    }

    // the final
    def S(data: Seq[Double]): Seq[Double] = {

      def s(data: Seq[Double]) = {
        val even = data.zipWithIndex.filter { case (d, i) => i % 2 == 0 }.map(_._1)
        val odd = data.zipWithIndex.filter { case (d, i) => i % 2 == 1 }.map(_._1)
        even ++ odd
      }

      val n = data.length
      val sTimes = log2Up(n) - 1

      var ret = data
      (0 until sTimes).foreach(_ => ret = ret.t(s))
      ret
    }

    def I(data: Seq[Double]) = data

    // butterfly
    def II(data: Seq[Double]) = {
      val n = data.length
      val as = data.take(n / 2)
      val bs = data.takeRight(n / 2)
      as.zip(bs.reverse).map { case (a, b) => a + b } ++ as.zip(bs.reverse).map { case (a, b) => a - b }
    }

    // multiplications
    def M(data: Seq[Double]) = {
      val n = data.length
      val coeffs = (0 until n).map(i => 2 * cos((2 * i + 1) * Pi / (4 * n)))
      data.zip(coeffs).map { case (d, coeff) => d * coeff }
    }

    // multiplications
    def R(data: Seq[Double]) = {
      val n = data.length
      val coeffs = (0 until n).map(i => 1 / (2 * cos((2 * i + 1) * Pi / (4 * n))))
      data.zip(coeffs).map { case (d, coeff) => d * coeff }
    }

    // additions
    def T(data: Seq[Double]) = {

      val n = data.length

      def trans(data: Seq[Double], i: Int) = {
        val coeffs = ((0.5 +: (0 until i).map(i => pow(-1.0, i - 1))) ++ Seq.fill(n - i - 1)(0.0)).map(_ * pow(-1.0, i))
        data.zip(coeffs).map { case (d, coeff) => d * coeff }.sum
      }

      (0 until n).map(i => trans(data, i))
    }

    // additions
    def A(data: Seq[Double]) = data.zip(data.tail :+ 0.0).map { case (a, b) => a + b }

    new DenseVector(build(data.toArray).toArray)
  }
}
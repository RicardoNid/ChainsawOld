package Chainsaw.algos

import Chainsaw._
import breeze.linalg.{DenseVector, product, sum}
import breeze.math.Semiring
import breeze.math.i
import breeze.numerics.exp
import breeze.numerics.constants.Pi
import scala.reflect.ClassTag

/** definition of generic DFT and its variations
 *
 */
object Dft {

  /** generic DFT which can be implemented in multiple different domain
   *
   * @param omega N-th root of unity in the domain
   * @param unity multiplication unity in the domain
   * @param mult  multiplication operation
   * @param add   addition operation
   * @see ''Fast Algorithms for Signal Processing'' Chap1.4
   */
  @definition
  def genericDft[T](data: DenseVector[T], omega: T, inverse: Boolean = false)
                   (implicit semiring: Semiring[T], classTag: ClassTag[T]) = {

    val N = data.length
    val factors = semiring.one +: (1 to N).map(i => product(DenseVector.fill(i)(omega)))

    // vector style, which use var
    DenseVector.tabulate(N) { k =>
      val indices = (0 until N).map(i => if (inverse) (i * k) % N else -(i * k) % N + N)
      val coeffs = DenseVector.tabulate(N)(i => factors(indices(i)))
      sum(data *:* coeffs)
    }
  }

  @definition
  def dft(data: DenseVector[BComplex], inverse: Boolean = false): DenseVector[BComplex] = {
    val N = data.length
    val omega = exp((2 * Pi / N) * i)
    genericDft(data, omega, inverse)
  }

  val idft: DenseVector[BComplex] => DenseVector[BComplex] = dft(_, inverse = true)

  def fftSymmetricOf(data: DenseVector[BComplex]) =
    new DenseVector((data(0) +: data.toArray.tail.reverse).map(_.conjugate))

  /** improve the performance of real-valued dft by doubling
   *
   * @param data0 a real-valued
   * @param data1 another sequence with same properties
   * @see ''Fast Algorithms for Signal Processing'' Chap5.1
   */
  @fastAlgo("dft")
  def rvdftByDouble(data0: DenseVector[Double], data1: DenseVector[Double]): (DenseVector[BComplex], DenseVector[BComplex]) = {
    val dataIn = new DenseVector(data0.toArray.zip(data1.toArray).map { case (real, imag) => BComplex(real, imag) })
    val dataOut = dft(dataIn)
    val out0 = (dataOut + fftSymmetricOf(dataOut)) / BComplex(2.0, 0.0)
    val out1 = (dataOut - fftSymmetricOf(dataOut)) / BComplex(0.0, 2.0)
    (out0, out1)
  }

  /** improve the performance of hermitian symmetric idft by doubling
   *
   * @param data0 a hermitian symmetric sequence
   * @param data1 another sequence with same properties
   * @see ''Fast Algorithms for Signal Processing'' Chap5.1
   */
  @fastAlgo("dft")
  def rvidftByDouble(data0: DenseVector[BComplex], data1: DenseVector[BComplex]): (DenseVector[Double], DenseVector[Double]) = {
    val dataIn = data0 + (data1 *:* i)
    val dataOut = idft(dataIn)
    val out0 = dataOut.map(_.real)
    val out1 = dataOut.map(_.imag)
    (out0, out1)
  }


  /** radix-2 fft
   *
   * @param DIT decimation-in-time/decimation-in-frequency
   */
  @fastAlgo("genericDft")
  def genericR2Fft[T](data: DenseVector[T], omega: T, inverse: Boolean = false, DIT: Boolean = false)
                     (implicit semiring: Semiring[T], classTag: ClassTag[T]): DenseVector[T] = ??? // TODO

  /** the data path of real-valued fft
   *
   * @see ''Pipelined Architectures for Real-Valued FFT and Hermitian-Symmetric IFFT With Real Datapaths'' [[https://www.semanticscholar.org/paper/Pipelined-Architectures-for-Real-Valued-FFT-and-Salehi-Amirfattahi/d67de58011d0c403682a55471f5adec702acdf0c]]
   * @param omega
   */
  @hardAlgo("dft")
  def r2rvfft(data: DenseVector[Double], inverse: Boolean) {

  }
}

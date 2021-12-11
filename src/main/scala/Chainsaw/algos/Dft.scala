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
    val omega = exp(-(2 * Pi / N) * i)
    genericDft(data, omega, inverse)
  }

  /** radix-2 fft
   * @param DIT decimation-in-time/decimation-in-frequency
   */
  @fastAlgo
  def genericR2Fft[T](data: DenseVector[T], omega: T, inverse: Boolean = false, DIT: Boolean = false)
                     (implicit semiring: Semiring[T], classTag: ClassTag[T]):DenseVector[T]= ??? // TODO

  /** the data path of real-valued fft
   * @see ''Pipelined Architectures for Real-Valued FFT and Hermitian-Symmetric IFFT With Real Datapaths'' [[https://www.semanticscholar.org/paper/Pipelined-Architectures-for-Real-Valued-FFT-and-Salehi-Amirfattahi/d67de58011d0c403682a55471f5adec702acdf0c]]
   * @param omega
   */
  @hardAlgo("dft")
  def r2rvfft(data:DenseVector[Double], inverse:Boolean){

  }
}

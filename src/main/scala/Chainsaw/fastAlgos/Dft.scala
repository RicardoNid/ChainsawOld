package Chainsaw.fastAlgos

import Chainsaw._
import breeze.linalg.DenseVector

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
  def genericDft[T](data: Array[T], omega: T, unity: T, mult: (T, T) => T, add: (T, T) => T, inverse: Boolean = false) = {
    val N = data.length
    val coeffs = unity +: (1 to N).map(i => Seq.fill(i)(omega).reduce(mult))
    require(coeffs.distinct.size == N && coeffs.last == coeffs.head)
    (0 until N).map(k =>
      (0 until N).map { i =>
        val index = if (inverse) (i * k) % N else (-(i * k) % N) + N
        mult(coeffs(index), data(i))
      }.reduce(add)
    )
  }

  import breeze.math.i
  import breeze.numerics.exp
  import breeze.numerics.constants.Pi

  @definition
  def dft(data: Array[BComplex], inverse: Boolean = false) = {

    val N = data.length
    val omega = exp(-(2 * Pi / N) * i)
    val unity = BComplex(1, 0)
    val mult = (a: BComplex, b: BComplex) => a * b
    val add = (a: BComplex, b: BComplex) => a + b

    genericDft(data, omega, unity, mult, add, inverse).toArray
  }

  import cc.redberry.rings
  import rings.scaladsl._
  import Chainsaw.ringsUtils._

}

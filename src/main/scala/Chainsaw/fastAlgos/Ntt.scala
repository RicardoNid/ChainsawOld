package Chainsaw.fastAlgos

import Chainsaw.fastAlgos.Dft.genericDft
import Chainsaw.ringsUtils.ZpUtil
import breeze.linalg.DenseVector
import cc.redberry.rings.scaladsl.Ring

object Ntt {

  /** number theoretic transform
   * @param ring the ring at which ntt is defined
   */
  @definition
  def ntt(data: DenseVector[Int], inverse: Boolean = false)(implicit ring: Ring[Long]): DenseVector[Int] = {

    val N = data.length
    val p = ring.cardinality().intValue()
    val omega = ring.getNthRoot(N).toInt
    val unity = 1
    val mult = (a: Int, b: Int) => (a * b) % p
    val add = (a: Int, b: Int) => (a + b) % p

    new DenseVector(genericDft(data.toArray, omega, unity, mult, add, inverse).toArray)
  }

  def intt(data: DenseVector[Int])(implicit ring: Ring[Long]): DenseVector[Int] = ntt(data, inverse = true)

  @fastAlgo
  def cconvByNtt(a: DenseVector[Int], b: DenseVector[Int])(implicit ring: Ring[Long]): DenseVector[Int] = intt(ntt(a) *:* ntt(b))
}

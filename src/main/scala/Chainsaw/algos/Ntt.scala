package Chainsaw.algos

import Chainsaw.algos.AlgebraicStructures.Zp
import Chainsaw.algos.Dft.genericDft
import breeze.linalg.DenseVector

object Ntt {

  def getNthRoot(modulus: Int, N: Int): Int = {
    import cc.redberry.rings.primes.BigPrimes
    import cc.redberry.rings.scaladsl.{Zp64, Ring}
    // TODO: better algo
    require(BigPrimes.isPrime(modulus) && (modulus - 1) % N == 0, "p is prime & p - 1 % N <=> N-th root of exists") // TODO: true?
    val ring: Ring[Long] = Zp64(modulus)
    val ret = (2 until modulus).filter { root =>
      ring.pow(root, N) == ring(1) && // N-th root
        (1 to N).map(ring.pow(root, _)).distinct.size == N
    }.head
    ret
  }

  /** number theoretic transform
   *
   * @param ring the ring at which ntt is defined
   */
  @definition
  def ntt(data: DenseVector[Int], inverse: Boolean = false)(implicit zp: Zp): DenseVector[Int] = {

    val N = data.length
    val modulus = zp.modulus
    val omega = getNthRoot(modulus, N)

    new DenseVector(genericDft(data, omega).toArray)
  }

  def intt(data: DenseVector[Int])(implicit zp: Zp): DenseVector[Int] = ntt(data, inverse = true)

  @fastAlgo
  def cconvByNtt(a: DenseVector[Int], b: DenseVector[Int])(implicit zp: Zp): DenseVector[Int] = intt(ntt(a) *:* ntt(b))

  @fastAlgo
  def multByNtt(a: BigInt, b: BigInt): BigInt = ??? // TODO
}

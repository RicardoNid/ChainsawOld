package Chainsaw.algos

import Chainsaw.algos.AlgebraicStructures.SemiZp
import Chainsaw.algos.Dft.genericDft
import Chainsaw.{definition, fastAlgo}
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
  def ntt(data: DenseVector[Int], inverse: Boolean = false)(implicit zp: SemiZp): DenseVector[Int] = {

    val N = data.length
    val modulus = zp.modulus
    val omega = getNthRoot(modulus, N)

    val ret = genericDft(data, omega)
    println(ret)
    ret
  }

  def intt(data: DenseVector[Int])(implicit zp: SemiZp): DenseVector[Int] = ntt(data, inverse = true)

  @fastAlgo("cconv")
  def cconvByNtt(a: DenseVector[Int], b: DenseVector[Int])(implicit zp: SemiZp): DenseVector[Int] = intt(ntt(a) *:* ntt(b))

  @fastAlgo("")
  def multByNtt(a: BigInt, b: BigInt): BigInt = ??? // TODO
}

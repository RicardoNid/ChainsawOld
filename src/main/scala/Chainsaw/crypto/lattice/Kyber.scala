package Chainsaw.crypto.lattice

import Chainsaw.crypto._
import Chainsaw.{crypto, logger}
import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import spinal.core.isPow2
import Chainsaw.ringsUtils._

/**
 * @see High-Speed NTT-based Polynomial Multiplication Accelerator for CRYSTALS-Kyber Post-Quantum Cryptography [[https://eprint.iacr.org/2021/563.pdf]]
 */
object Kyber {

  /** NTT, but we use K-representation instead of the original values
   */
  def KNTT(coeffs: Seq[Long], inverse: Boolean = false)(implicit ring: Ring[Long]): Seq[Long] = {

    val N: Int = coeffs.size
    require(isPow2(N))
    val NInverse = ring.inverseOf(N)

    // get k
    val (p, m, k) = crypto.ModularReduction.getKREDConfig(ring)
    val k2 = k * k
    val k2Inverse = ring.inverseOf(k2)

    val omega = crypto.NTT(ring, N).omega

    // preprocessing, to have data and twiddle factors in K-representation
    val preprocessedCoeffs = coeffs.map(_ * k2Inverse)
    def getTwiddle(index: Int): Long = ring(ring.pow(omega, index) * k2Inverse)

    // all the multiplications become multiplication + K2RED
    def k2Mult(a:Long, b:Long): Long = ring(a * b * k2)

    // original NTT
    val ret = (0 until N).map { k =>
      if (!inverse) ring(preprocessedCoeffs.zipWithIndex.map { case (value, i) => k2Mult(value, getTwiddle(i * k)) }.sum)
      else ring(preprocessedCoeffs.zipWithIndex.map { case (value, i) => k2Mult(value, getTwiddle(-i * k)) * NInverse }.sum )
    }

    // postprocessing, to get them back from K-representation
    val finalRet = ret.map(value => ring(value * k2))

    val inverseString = if (inverse) "inverse" else ""
    logger.debug(s"$inverseString NTT:\nN:$N, N^-1:, omega:$omega\ninput:  ${coeffs.mkString(" ")}\nresult: ${finalRet.mkString(" ")}")

    finalRet
  }
}

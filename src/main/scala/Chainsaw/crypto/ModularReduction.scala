package Chainsaw.crypto

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

import Chainsaw.ringsUtils._

object ModularReduction {

  /** modular reduction for a specific Zp in which p = k * 2^m^ + 1, like Montgomery modular reduction, this won't give an exact mod result, so you need regard it as a different "representation"
   *
   * @see Speeding up the Number Theoretic Transform for Faster Ideal Lattice-Based Cryptography [[http://eprint.iacr.org/2016/504.pdf]]
   *      for original KRED
   * @see High-Speed NTT-based Polynomial Multiplication Accelerator for CRYSTALS-Kyber Post-Quantum Cryptography [[https://eprint.iacr.org/2021/563.pdf]]
   *      for K2RED
   * @see [[]] for authors explanation // TODO: write this
   * @see [[crypto]] for hardware implementation
   * @return ret === K^2^ * c mod p
   */
  def getKREDConfig(ring: Ring[Long]) = {
    val p = ring.p
    val m = (p - 1).toBinaryString.reverse.takeWhile(_ == '0').size
    val k = (p - 1) / (1 << m)
    require(p == k * (1 << m) + 1, s"K2RED require p = k * 2^m + 1, while $p != $k * 2^$m + 1")
    (p, m, k)
  }

  def K2RED(c: Int, ring: Ring[Long]): Long = { // TODO: generalize this for different q
    // get m and k from p
    val (p, m, k) = getKREDConfig(ring)
    val factor = 1 << m

    val cl = c % factor
    val ch = c / factor
    val cPrime = k * cl - ch
    require(ch * factor + cl == c)

    val cPrimeSize = cPrime.toBinaryString.dropWhile(_ == '1').size + 1

    val cPrimel = BigInt(cPrime.toBinaryString.takeRight(8), 2).toLong // c7 -> 0
    val cPrimeh = scala.math.floor(cPrime / factor.toDouble).toLong // c15 -> c8
    val cPrime2 = k * cPrimel - cPrimeh

    logger.debug(s"K2RED: c $c, cl $cl, ch $ch, cPrime $cPrime, cPrimeh $cPrimeh, cPrimel $cPrimel, ret $cPrime2")
    require(cPrimeSize <= 17, s"$ch, $cl") // extra bit(16 + 1) for sign extension
    require(cPrimeh * factor + cPrimel == cPrime)

    val ret = if (cPrime2 - p >= 0) cPrime2 - p
    else if (cPrime2 < 0) cPrime2 + p
    else cPrime2

    require(ring.isCorrect(ret))
    ret
  }

}

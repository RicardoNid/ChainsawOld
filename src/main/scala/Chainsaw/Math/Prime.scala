package Chainsaw.Math

import Chainsaw.Math.Prime.primeFactors

import scala.collection.mutable.ArrayBuffer
import cc.redberry.rings.primes.SmallPrimes
import cc.redberry.rings.primes.BigPrimes
import cc.redberry.rings.bigint.BigInteger

object Prime {

  // TODO: expand for BigInt
  def primeFactors(n: Int) = SmallPrimes.primeFactors(n)

  def isPrime(n: Int): Boolean = SmallPrimes.isPrime(n)

  /** The next prime number after n
   *
   * @example nextPrime(13) = 17
   */
  def nextPrime(n: Int): Long =  SmallPrimes.nextPrime(n)

  def main(args: Array[String]): Unit = {
    println(isPrime(13))
  }
}
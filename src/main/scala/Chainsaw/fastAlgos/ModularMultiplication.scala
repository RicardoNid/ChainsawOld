package Chainsaw.fastAlgos

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import cc.redberry.rings.primes

/** modular multiplication algorithms, they validate themselves while running
 *
 */
object ModularMultiplication {

  def viewBigInt(x: BigInt) = {
    if (x < 0) println("negative")
    println(s"width: ${x.bitLength}")
  }

  /** Montgomery modular multiplication
   *
   * @param N modulo
   * @return xyR^-1^ mod N
   * @see ''Modular multiplication without trial division'' [[https://www.ams.org/mcom/1985-44-170/S0025-5718-1985-0777282-X/S0025-5718-1985-0777282-X.pdf]]
   */
  def mmm(x: BigInt, y: BigInt, N: BigInt): BigInt = {

    // use number length of current crypto system
    //    val lN = nextPower2(N.bitLength).toInt
    // or, use the nearest power of 2
    val lN = N.bitLength

    // preparing parameters
    // TODO: a propre R for RSA?
    val R = BigInt(1) << lN
    // TODO: algo to get RPrime & NPrime
    val RInverse = R.modInverse(N)
    val NPrime = (R * RInverse - 1) / N

    // calculation
    val T = x * y
    val m = ((T % R) * NPrime) % R
    val t = (T + m * N) / R

    val ret = if (t >= N) t - N else t // t \in [0, 2N)
    assert(ret == (x * y * RInverse) % N)
    ret
  }

  /** McLaughlin Montgomery modular multiplication
   *
   * @see ''NEW FRAMEWORKSFOR MONTGOMERY'S MODULAR MULTIPLICATIONMETHOD'' variation 2 [[https://www.ams.org/journals/mcom/2004-73-246/S0025-5718-03-01543-6/S0025-5718-03-01543-6.pdf]]
   */
  def mlm(a: BigInt, b: BigInt, N: BigInt): BigInt = {

    // use number length of current crypto system
    //    val lN = nextPower2(N.bitLength).toInt
    // or, use the nearest power of 2
    val lN = N.bitLength

    // preparing parameters
    val R = (BigInt(1) << lN) - 1
    // TODO: verify that GCD(R, N) == 1
    val QPrime = (BigInt(1) << lN) + 1
    val RInverse = R.modInverse(N)
    val NPrime = (R * RInverse - 1) / N

    // calculation
    val m = (a * b * NPrime) % R
    val S = (a * b + m * N) % QPrime
    val w = -S % QPrime + QPrime // or, scala would generate negative value

    // conditional selection
    val s = if (w % 2 == 0) w / 2 else (w + QPrime) / 2
    val t = if ((s % 2) == ((a * b + m * N) % 2)) s else s + QPrime
    val ret = if (t >= N) t - N else t // t \in [0, 2N)

    assert(ret == (a * b * RInverse) % N)
    ret
  }

  /** Improved McLaughlin Montgomery modular multiplication, avoid conditional selections by setting new (looser) bounds and thus consume more width
   *
   * @param x \in [0, 2N)
   * @param y \in [0, 2N)
   * @return ret = xyR^-1^ (mod N) \in [0, 2N)
   * @see ''FFT-Based McLaughlin’s MontgomeryExponentiation without Conditional Selections'' [[http://cetinkoc.net/docs/j81.pdf]]
   */
  def mlws(x: BigInt, y: BigInt, N: BigInt): BigInt = {

    val lN = N.bitLength + 2 // s,t r,h > 4n

    // preparing parameters
    val r = (BigInt(1) << lN) - 1 // corresponding to R in MLM
    // TODO: verify that GCD(R, N) == 1
    val h = (BigInt(1) << lN) + 1 // corresponding to QPrime in MLM
    val RInverse = r.modInverse(N)
    val NPrime = (r * RInverse - 1) / N

    // calculation
    val m = (x * y * NPrime) % r
    val g = (x * y + m * N) % h
    val w = (h - g) / 2 // or, scala would generate negative value

    val ret = w // r \in [0, 2N)
    assert((ret % N) == (x * y * RInverse) % N)
    ret
  }

  /**
   * @see ''Discrete weighted transforms and large-integer arithmetic'' [[https://www.ams.org/mcom/1994-62-205/S0025-5718-1994-1185244-1/S0025-5718-1994-1185244-1.pdf]]
   */
  def fftms(x: BigInt, y: BigInt, N: BigInt) = {}
}

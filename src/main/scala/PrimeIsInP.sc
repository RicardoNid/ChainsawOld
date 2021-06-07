
import cc.redberry.rings.primes._
import cc.redberry.rings._
import cc.redberry.rings.scaladsl._

import scala.math.{floor, log, sqrt}
import scala.annotation.tailrec
import scala.math._

SmallPrimes.millerRabinPrimeTest(14)

@tailrec
def gcd(n: Int, m: Int): Int = if (m == 0) n else gcd(m, n % m)

@tailrec
def getContinuedFraction(m: Int, n: Int, qs: Seq[Int] = Seq[Int]()): Seq[Int] =
  if (m % n == 0) qs :+ (m / n)
  else getContinuedFraction(n, m % n, qs :+ m / n)

getContinuedFraction(11, 9)
getContinuedFraction(294, 159)
getContinuedFraction(314159, 100000)
getFactorsByContinuedFraction(9073)

def getFactorsByFermat(n: Int) = {

}

def getFactorsByContinuedFraction(n: Int) = {
  def iter(bMinus1: Int, b: Int, a: Int, x: Double, times: Int = 0): Unit = {
    println(s"times: $times, a: $a, b: $b, x: $x")
    val newA = floor(1 / x).toInt
    val newX = 1 / x - newA
    val newB = (newA * b + bMinus1) % n

    val cand = (newB * newB) % n
    val modifiedCand = if (cand > n / 2) cand - n else cand
    println(s"candidate: $modifiedCand, ${SmallPrimes.primeFactors(abs(modifiedCand)).mkString(" ")}")

    if (times >= 10 || newX == 0) Unit
    else iter(b, newB, newA, newX, times + 1)
  }

  val initAB = floor(sqrt(n)).toInt
  val initX = sqrt(n) - initAB
  val cand = (initAB * initAB) % n

  val modifiedCand = if (cand > n / 2) cand - n else cand
  println(s"candidate: $modifiedCand, ${SmallPrimes.primeFactors(abs(modifiedCand)).mkString(" ")}")

  iter(1, initAB, initAB, initX)
}

def modByBarrett(x: Int, m: Int) = {
  val k = m.toBinaryString.length
  require(x.toBinaryString.length <= 2 * k + 1)
  val mu = (1 << (2 * k)) / m // precomputation

  val q1 = x >> (k - 1)
  val q2 = q1 * mu
  val q3 = q2 >> (k + 1)

  val r1 = x % (1 << (k + 1))
  val r2 = (q3 * m) % (1 << (k + 1))

  val s = if (r1 - r2 < 0) r1 - r2 + (1 << (k + 1)) else r1 - r2

  def reduction(s: Int): Int = if (s < m) s else reduction(s - m)

  val ret = reduction(s)
  ret
}

modByBarrett(3561, 47)
(0 to 4096).forall(i => modByBarrett(i, 47) == i % 47)

def modByMontgomery(t: Int, N: Int) = {
  val rho = 1 << N.toBinaryString.length
  require(t < N * (rho - 1))
  val Zrho = Zp(rho.toLong)
  val Z2 = Zp(2.toLong)
  val Zm = Zp(N.toLong)
  val RInverse = Zm.reciprocal(rho).intValue() // pseudo precomputation

//  // original
//  val omega = -Zrho.reciprocal(N).intValue() // precomputation
//  // caution: mod of negative number, so we add an extra rho
//  val U = rho + (t * omega) % rho // multiplication 1
//  val ret = (t + U * N) / rho // multiplication 2

  // bit-optimized
  val omega = -Z2.reciprocal(rho).intValue() // precomputation
  val UN = t.toBinaryString.zipWithIndex
      .map { case (char, i) => char.asDigit * omega % 2 * N * (1 << i)}
      .sum
  val ret = (t + UN) / rho

  def reduction(s: Int): Int = if (s < N) s else reduction(s - N)

  reduction(ret)
}

val RInverse = Zp(47.toLong).reciprocal(64).intValue()
(1 until (64 * 46)).forall(i => (i * RInverse) % 47 == modByMontgomery(i, 47))
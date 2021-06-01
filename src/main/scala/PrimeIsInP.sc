
import cc.redberry.rings.primes._
import cc.redberry.rings._
import cc.redberry.rings.scaladsl._

import scala.math.{floor, log, sqrt}
import scala.annotation.tailrec
import scala.math._

@tailrec
def gcd(n: Int, m: Int): Int = if (m == 0) n else gcd(m, n % m)

SmallPrimes.millerRabinPrimeTest(14)

@tailrec
def getContinuedFraction(m: Int, n: Int, qs: Seq[Int] = Seq[Int]()): Seq[Int] =
  if (m % n == 0) qs :+ (m / n)
  else getContinuedFraction(n, m % n, qs :+ m / n)

getContinuedFraction(11, 9)
getContinuedFraction(294, 159)
getContinuedFraction(314159, 100000)

def getFactorsByFermat(n:Int) = {

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

getFactorsByContinuedFraction(9073)

def modByBarrett(x: Int, m: Int) = {
  val k = m.toBinaryString.length
  require(x.toBinaryString.length <= 2 * k)
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
(0 until 3561).forall(i => modByBarrett(i, 47) == i % 47)


def modByMontgomery(T: Int, m: Int) = {
  val R = 1 << m.toBinaryString.length
  require(T < m * R)
  val ZR = Zp(R.toLong)
  val Zm = Zp(m.toLong)
  val mPrime = -ZR.reciprocal(m).intValue() // precomputation
  //  println(s"mPrime: $mPrime")
  val RInverse = Zm.reciprocal(R).intValue() // pseudo precomputation
  // caution: mod of negative number
  val U = R + (T * mPrime % R) // multiplication 1
  //  println(s"U: $U")

  def reduction(s: Int): Int = if (s < m
  ) s else reduction(s - m)
  val ret = (T + U * m) / R // multiplication 2
  reduction(ret)
}

val RInverse = Zp(47.toLong).reciprocal(64).intValue()
modByMontgomery(RInverse, 47) == RInverse * RInverse % 47
(1 until (64 * 46)).forall(i => (i * RInverse) % 47 == modByMontgomery(i, 47))

val Zm = Zp64(127.toLong)
Zm.buildCachedReciprocals()








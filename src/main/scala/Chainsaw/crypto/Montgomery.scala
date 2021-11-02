package Chainsaw.crypto

import cc.redberry.rings.primes._
import cc.redberry.rings._
import cc.redberry.rings.scaladsl._

import scala.math.{floor, log, sqrt}
import scala.annotation.tailrec
import scala.math._


class Montgomery {
  @tailrec
  private def gcd(n: Int, m: Int): Int = if (m == 0) n else gcd(m, n % m)

  def montRed(t: Int, N: Int) = {
    val lN = N.toBinaryString.length
    val rho = 1 << lN
    require(t < N * (rho - 1))
    val Zrho = Zp(rho.toLong)
    val Z2 = Zp(2.toLong)
    val Zm = Zp(N.toLong)
    val RInverse = Zm.reciprocal(rho).intValue() // pseudo precomputation

    // original
    val omega = -Zrho.reciprocal(N).intValue() // precomputation
    // caution: mod of negative number, so we add an extra rho
    // TODO: fix this
    val U = rho + (t * omega) % rho // multiplication 1
    val ret = (t + U * N) / rho // multiplication 2

    //bit-optimized
    val omega0 = -Z2.reciprocal(N).intValue() // precomputation

    var r = t
    (0 until lN).foreach { i =>
      val tBinary = r.toBinaryString.reverse.padTo(lN, '0')
      val temp = tBinary(i).asDigit * omega0 % 2
      val u = if (temp < 0) temp + 2 else temp
      r += u * N * (1 << i)
      println(r.toBinaryString.reverse.padTo(lN, '0'))
    }
    println(s"U * N = ${U * N}, UN = ${r - t}")
    val ret0 = r / rho

    def reduction(s: Int): Int = if (s < N) s else reduction(s - N)

    reduction(ret0)
  }
}

object Montgomery {

  def main(args: Array[String]): Unit = {

    val mont = new Montgomery

    val RInverse = Zp(47.toLong).reciprocal(64).intValue()
    mont.montRed(RInverse, 47) == RInverse * RInverse % 47
    //    assert((1 until (63 * 47)).forall(i => (i * RInverse) % 47 == mont.montRed(i, 47)))
    //    assert((1 until (63 * 47)).forall(i => (i * RInverse) % 47 == mont.montRed(i, 47)))

    val Zm = Zp64(127.toLong)
    Zm.buildCachedReciprocals()
  }
}

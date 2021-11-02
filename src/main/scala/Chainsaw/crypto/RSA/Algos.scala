package Chainsaw.crypto.RSA

import cc.redberry.rings.scaladsl._
import Chainsaw._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Algos(lN: Int) {

  val Rho = BigInt(1) << lN

  // ... with prefix ... has corresponding hardware implementation

  def bigAdd(a: BigInt, b: BigInt) = {
    a + b
  }

  def bigMod(a: BigInt, modulus: BigInt) = {
    require(modulus.toString(2).tail.forall(_ == '0')) // modulus should be a power of the base(2)
    BigInt(a.toString(2).takeRight(modulus.toString(2).size - 1), 2)
  }

  // TODO:
  def bigMult(a: BigInt, b: BigInt) = {
    a * b
  }

  def bigMultMod(a: BigInt, b: BigInt, modulus: BigInt) = {
    require(modulus.toString(2).tail.forall(_ == '0')) // modulus should be a power of the base(2)
    val aMod = bigMod(a, modulus)
    val bMod = bigMod(b, modulus)
    bigMod(bigMult(aMod, bMod), modulus)
  }

  // TODO:
  def bigSquare(a: BigInt) = {
    a * a
  }

  /** Get omega = -N^-1^ (mod 2^lN^) by Hensel's lemma
   *
   * @param N the modulus of RSA
   */
  def getOmega(N: BigInt, print: Boolean = false) = {
    val init = BigInt(1) // N^{-1} \pmod 2^1
    // lifting by Hensel's lemma
    @tailrec
    var count = 0

    def lift(s: BigInt, exp: Int): BigInt = {
      if (print) {
        printPadded(s"omega in progress ${count.toString.padToLeft(3, '0')}", s, lN)
        printPadded(s"reverse           ${count.toString.padToLeft(3, '0')}", s, lN, reverse = true)
        count += 1
      }
      if (exp == lN) s
      else {
        val liftedS = { // denoted as s'
          val remainder = bigMultMod(s, N, BigInt(1) << (exp + 1))
          if (remainder == 1) s
          // solution + (BigInt(1) << exp), put an 1 to the left
          else BigInt("1" + s.toString(2).padToLeft(exp, '0'), 2)
        }
        lift(liftedS, exp + 1)
      }
    }

    val ret = Rho - lift(init, 1)
    if (print) printPadded(s"omega in progress ${count.toString.padToLeft(3, '0')}", ret, lN)
    ret
  }

  /** Get rho^2^ (mod N) by iterative algorithm
   *
   */
  def getRhoSquare(N: BigInt, print: Boolean = false) = {
    var count = 0

    @tailrec
    def iter(value: BigInt, exp: Int): BigInt = {
      if (print) {
        printPadded(s"rhoSquare in progress ${count.toString.padToLeft(3, '0')}", value, lN)
        count += 1
      }

      def cal(value: BigInt) = {
        val det = (value << 1) - N
        if (det >= 0) det else value << 1
      }
      // cal would be executed for lN times
      if (exp == (lN * 2 + 1)) value
      else iter(cal(value), exp + 1)
    }
    // as modulus N has the same width as Rho(lN), iteration could start from (1 << (lN - 1))
    iter(BigInt(1) << (lN - 1), lN)
  }

  def printPadded(name: String, value: BigInt, lN: Int = 512, reverse: Boolean = false) = {
    val hex = value.toString(2).padToLeft(lN, '0')
      .grouped(4).toArray.map(BigInt(_, 2).toString(16))
      .mkString("")
    println(s"$name = ${if (reverse) hex.reverse else hex}")
  }

  def montRed(t: BigInt, N: BigInt, print: Boolean = false) = {
    require(t >= 0 && t <= N * Rho - 1)
    // TODO: is t necessarily to be 2 * lN long?
    //    printPadded("t", t, 2 * lN)
    val U = bigMultMod(t, getOmega(N), Rho)
    val mid = (t + bigMult(U, N)) >> lN // divided by Rho
    val det = mid - N
    if (print) {
      printPadded("t low 1_k    ", bigMod(t, Rho), lN)
      printPadded("t     0_k    ", t, 2 * lN)
      printPadded("omega * t 1_k", bigMult(bigMod(t, lN), getOmega(N)), 2 * lN)
      printPadded("U     2_0    ", U, lN)
      printPadded("UN    2_k    ", bigMult(U, N), 2 * lN)
      printPadded("mid   2_k    ", mid, lN)
      //      printPadded("det", det, lN)
    }
    if (det >= 0) det else mid // result \in [0, N)
  }

  // montMul(aMont, bMont) = abMont
  def montMul(aMont: BigInt, bMont: BigInt, N: BigInt, print: Boolean = false) = {
    require(aMont >= 0 && aMont < N)
    require(bMont >= 0 && bMont < N)
    if (print) {
      println()
      printPadded("aMont 1_0   ", aMont)
      printPadded("bMont 1_0   ", bMont)
    }
    // aMont, bMont \in [0, N), aMont * bMont \in [0 N^2), N^2 < N * Rho - 1(as N   < Rho)
    val prod = bigMult(aMont, bMont)
    montRed(prod, N, print)
  }

  def montSquare(aMont: BigInt, N: BigInt, print: Boolean = false) = {
    require(aMont >= 0 && aMont < N)
    if (print) printPadded("aMont    ", aMont)
    val square = bigSquare(aMont)
    montRed(square, N, print)
  }

  def montExp(a: BigInt, exponent: BigInt, N: BigInt, print: Boolean = false) = {
    require(a >= 0 && a < N)
    val aMont = montMul(a, getRhoSquare(N), N, print)
    //    printPadded("aMont", aMont)
    val sequence = exponent.toString(2)
    var reg = aMont
    sequence.tail.foreach { char =>
      val square = montSquare(reg, N, print = print)
      if (char == '1') {
        reg = montMul(square, aMont, N, print = print)
      }
      else reg = square
    }
    montRed(reg, N, print = print)
  }

  def R2MM(X: BigInt, Y: BigInt, N: BigInt) = {
    var S = BigInt(0)
    val lN = N.bitLength
    val y0 = Y.toString(2).last.asDigit
    for (i <- 0 until lN) {
      val s0 = S.toString(2).last.asDigit
      val xi = X.toString(2).padToLeft(lN, '0').reverse(i).asDigit
      val qi = (xi * y0) ^ s0
      S = (S + xi * Y + qi * N) / 2
    }
    if (S >= N) S - N else S
  }
}

object Algos {
  def apply(lN: Int): Algos = new Algos(lN)
  def main(args: Array[String]): Unit = {
    val algo = new Algos(4)
    val a = 7
    val b = 8
    println(algo.R2MM(a,b,13))
    val RhoReverse = Zp64(13).reciprocal(16)
    println(s"golden = ${a * b * RhoReverse % 13}")
  }
}
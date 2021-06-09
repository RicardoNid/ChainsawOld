package Chainsaw.Crypto.RSA

import cc.redberry.rings.scaladsl._
import Chainsaw._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class RSAAlgo(lN: Int) {

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
  def getOmega(N: BigInt) = {
    val init = BigInt(1) // N^{-1} \pmod 2^1
    // lifting by Hensel's lemma
    @tailrec
    var count = 0

    def lift(s: BigInt, exp: Int): BigInt = {
      if (ChainsawDebug) {
        printPadded(s"omega in progress ${count.toString.padToLeft(3, '0')}", s, lN)
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
    if (ChainsawDebug) printPadded(s"omega in progress ${count.toString.padToLeft(3, '0')}", ret, lN)
    ret
  }

  /** Get rho^2^ (mod N) by iterative algorithm
   *
   */
  def getRhoSquare(N: BigInt, print:Boolean = false) = {
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

  def printPadded(name: String, value: BigInt, lN: Int = 512) = {
    val hex = value.toString(2).padToLeft(lN, '0')
      .grouped(4).toArray.map(BigInt(_, 2).toString(16))
      .mkString("")
    println(s"$name = $hex")


  }

  def montRed(t: BigInt, N: BigInt, print: Boolean = false) = {
    require(t >= 0 && t <= N * Rho - 1)
    // TODO: is t necessarily to be 2 * lN long?
    //    printPadded("t", t, 2 * lN)
    val U = bigMultMod(t, getOmega(N), Rho)
    val mid = (t + bigMult(U, N)) >> lN // divided by Rho
    val det = mid - N
    if (print) {
      printPadded("t low    ", bigMod(t, Rho), lN)
      printPadded("t        ", t, 2 * lN)
      printPadded("U        ", U, lN)
      printPadded("omega * t", bigMult(t, getOmega(N)), 2 * lN)
      printPadded("UN       ", bigMult(U, N), 2 * lN)
      printPadded("mid      ", mid, lN)
      //      printPadded("det", det, lN)
    }
    if (det >= 0) det else mid // result \in [0, N)
  }

  // montMul(aMont, bMont) = abMont
  def montMul(aMont: BigInt, bMont: BigInt, N: BigInt, print: Boolean = false) = {
    require(aMont >= 0 && aMont < N)
    require(bMont >= 0 && bMont < N)
    if (print) {
      printPadded("aMont    ", aMont)
      printPadded("bMont    ", bMont)
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
      val square = montSquare(reg, N, print = false)
      //      printPadded("afterSquare", square)
      if (char == '1') {
        reg = montMul(square, aMont, N, print = false)
        //        printPadded("afterMul", reg)
      }
      else reg = square
    }
    montRed(reg, N, print = false)
  }

  def montExpWithRecord(a: BigInt, exponent: BigInt, N: BigInt) = {
    require(a >= 0 && a < N)
    val record = ArrayBuffer[BigInt]()
    val aMont = montMul(a, getRhoSquare(N), N)
    record += aMont
    val sequence = exponent.toString(2)
    var reg = aMont
    sequence.tail.foreach { char =>
      val square = montSquare(reg, N)
      record += square
      if (char == '1') {
        reg = montMul(square, aMont, N)
        record += reg
      }
      else reg = square
    }
    val ret = montRed(reg, N)
    record += ret
    record
  }
}
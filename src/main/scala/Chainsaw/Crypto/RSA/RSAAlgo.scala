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
  def getOmega(N: BigInt, print: Boolean = false) = {
    val init = BigInt(1) // N^{-1} \pmod 2^1
    // lifting by Hensel's lemma
    @tailrec
    var count = 0

    def lift(solution: BigInt, exp: Int): BigInt = {
      if (print) {
        printPadded(s"omega in progress ${count.toString.padToLeft(3, '0')}", solution, lN)
        count += 1
      }
      if (exp == lN) solution
      else {
        val liftedSolution = {
          val remainder = bigMultMod(solution, N, BigInt(1) << (exp + 1))
          if (remainder == 1) solution
          // solution + (BigInt(1) << exp), put an 1 to the left
          else BigInt("1" + solution.toString(2).padToLeft(exp, '0'), 2)
        }
        lift(liftedSolution, exp + 1)
      }
    }

    val ret = Rho - lift(init, 1)
    if(print) printPadded(s"omega in progress ${count.toString.padToLeft(3, '0')}", ret, lN)
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

  def printPadded(name: String, value: BigInt, lN: Int = 512) = {
    val hex = value.toString(2).padToLeft(lN, '0')
      .grouped(4).toArray.map(BigInt(_, 2).toString(16))
      .mkString("")
    println(s"$name = $hex")
  }

  def montRed(t: BigInt, N: BigInt) = {
    require(t >= 0 && t <= N * Rho - 1)
    // TODO: is t necessarily to be 2 * lN long?
    //    printPadded("t", t, 2 * lN)
    val U = bigMultMod(t, getOmega(N), Rho)
    //    printPadded("U", U)
    val mid = (t + bigMult(U, N)) >> lN // divided by Rho
    //    printPadded("UN", bigMult(U, N), 2 * lN)
    //    printPadded("mid", mid, lN)
    val det = mid - N
    //    printPadded("det", det, lN)
    if (det >= 0) det else mid // result \in [0, N)
  }

  // montMul(aMont, bMont) = abMont
  def montMul(aMont: BigInt, bMont: BigInt, N: BigInt) = {
    require(aMont >= 0 && aMont < N)
    require(bMont >= 0 && bMont < N)
    // aMont, bMont \in [0, N), aMont * bMont \in [0 N^2), N^2 < N * Rho - 1(as N   < Rho)
    val prod = bigMult(aMont, bMont)
    montRed(prod, N)
  }

  def montSquare(aMont: BigInt, N: BigInt) = {
    require(aMont >= 0 && aMont < N)
    val square = bigSquare(aMont)
    montRed(square, N)
  }

  def montExp(a: BigInt, exponent: BigInt, N: BigInt) = {
    require(a >= 0 && a < N)
    val aMont = montMul(a, getRhoSquare(N), N)
    //    printPadded("aMont", aMont)
    val sequence = exponent.toString(2)
    var reg = aMont
    sequence.tail.foreach { char =>
      val square = montSquare(reg, N)
      //      printPadded("afterSquare", square)
      if (char == '1') {
        reg = montMul(square, aMont, N)
        //        printPadded("afterMul", reg)
      }
      else reg = square
    }
    montRed(reg, N)
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

object RSAAlgo {
  def main(args: Array[String]): Unit = {

    // test for getOmega
    val testSize = 512
    val algo = new RSAAlgo(testSize)
    val ref = new RSARef(testSize)
    val Zrho = Zp(asBigInteger(BigInt(1) << testSize))

    def assertBig(a: BigInt, b: IntZ) = assert(a.toString(2) == b.toString(2),
      s"\nyours:  ${a.toString(2)}, \ngolden: ${b.toString(2)}")

    (0 until 10).foreach { _ =>
      val modulus = ref.getModulus
      //      println(s"the size of modulus is always ${modulus.toString(2).size}")
      val omega = algo.getOmega(modulus)
      val goldenOmega: IntZ = Zrho.reciprocal(modulus)
      assertBig(algo.bigMultMod(omega, modulus, algo.Rho), algo.Rho - 1)
      assertBig(-omega + algo.Rho, goldenOmega)
      ref.refresh()
    }
    printlnGreen(s"getOmega, passed")

    (0 until 10).foreach { _ =>
      val modulus = ref.getModulus
      val ZN = Zp(modulus)
      assertBig(algo.getRhoSquare(modulus), ZN(BigInt(1) << 1024))
      ref.refresh()
    }
    printlnGreen(s"getRhoSquare, passed")

    (0 until 10).foreach { _ =>
      val modulus = ref.getModulus
      val ZN = Zp(modulus)
      val input = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
      val RhoInverse = ZN.reciprocal(algo.Rho)
      assertBig(algo.montRed(input, modulus), ZN.multiply(input, RhoInverse))
      ref.refresh()
    }
    printlnGreen(s"montRed, passed")

    //    val newAlgo = new RSAAlgo(4)
    //    val ZN = Zp(13)
    //    val RhoInvers = ZN.reciprocal(newAlgo.Rho)
    //    println(s"RhoInvers = $RhoInvers")
    //    println(newAlgo.montRed(4, 13))
    //    println(ZN.multiply(4, RhoInvers))

    (0 until 10).foreach { _ =>
      val modulus = ref.getModulus
      val publicKey = ref.getPublicValue
      //    val input = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
      val input = BigInt(("a" * 64).getBytes())
      //      println(input.toString(2).size)
      val ZN = Zp(modulus)
      assertBig(algo.montExp(input, publicKey, modulus), ZN.pow(input, publicKey))
      ref.refresh()
    }
    printlnGreen(s"montExp, passed")
  }
}

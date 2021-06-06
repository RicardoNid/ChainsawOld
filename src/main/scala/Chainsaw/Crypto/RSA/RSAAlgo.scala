package Chainsaw.Crypto.RSA

import cc.redberry.rings.scaladsl._
import Chainsaw._

class RSAAlgo(lN: Int) {

  val Rho = BigInt(1) << lN

  // ... with prefix ... has corresponding hardware implementation

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
    def lift(solution: BigInt, exp: Int): BigInt = {
      if (exp == lN) solution
      else {
        val liftedSolution = {

          // version 1
          //          if (solution * N % (BigInt(1) << (exp + 1)) == 1) solution
          //          else solution + (BigInt(1) << exp)

          // version 2 clarify
          //          val product = bigMult(solution, N)
          //          val remainder = BigInt(product.toString(2).takeRight(exp + 1)) // % 2^(exp + 1)

          // version 3 merge multiplication and mod,implement it by a specific hardware later
          val remainder = bigMultMod(solution, N, BigInt(1) << (exp + 1))

          if (remainder == 1) solution
          //          else solution + (BigInt(1) << exp)
          else BigInt("1" + solution.toString(2).padToLeft(exp, '0'), 2) // put a 1 to the left

        }
        //        println(s"lifted: $liftedSolution")
        lift(liftedSolution, exp + 1)
      }
    }

    Rho - lift(init, 1)
  }

  /** Get rho^2^ (mod N) by iterative algorithm
   *
   */
  def getRhoSquare(N: BigInt) = {

    def iter(value: BigInt, exp: Int): BigInt = {

      // version 1
      //      def cal(value: BigInt) = 2 * value % N

      // version 2
      def cal(value: BigInt) = {
        val det = (value << 1) - N
        if (det > 0) det else det + N
      }

      if (exp == (lN << 1)) cal(value)
      else iter(cal(value), exp + 1)
    }

    // version 1,2
    //    iter(1, 1)

    // version 3: as modulus N has the same width as Rho(lN), iteration could start from (1 << (lN - 1))
    iter(BigInt(1) << (lN - 1), lN)
  }

  def montRed(t: BigInt, N: BigInt) = {
    require(t >= 0 && t <= N * Rho - 1)
    // TODO: is t necessarily to be 2 * lN long?
    val U = bigMultMod(t, getOmega(N), Rho)
    // t + bigMult(U, N) carry? may be!
    val mid = (t + bigMult(U, N)) >> lN // divided by Rho
    val det = mid - N
    if (det >= 0) det else mid // result \in [0, N)
  }

  // montMul(aMont, bMont) = abMont
  def montMul(aMont: BigInt, bMont: BigInt, N: BigInt) = {
    require(aMont >= 0 && aMont < N)
    require(bMont >= 0 && bMont < N)
    // aMont, bMont \in [0, N), aMont * bMont \in [0 N^2), N^2 < N * Rho - 1(as N < Rho)
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
    val sequence = exponent.toString(2)
    var reg = aMont
    sequence.tail.foreach { char =>
      val square = montSquare(reg, N)
      if (char == '1') reg = montMul(square, aMont, N)
      else reg = square
    }
    montRed(reg, N)
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

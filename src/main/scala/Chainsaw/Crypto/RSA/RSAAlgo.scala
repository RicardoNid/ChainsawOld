package Chainsaw.Crypto.RSA

import cc.redberry.rings.scaladsl._
import Chainsaw._

class RSAAlgo(lN: Int) {

  val Rho = BigInt(1) << lN

  def bigMult(a: BigInt, b: BigInt) = {
    a * b
  }

  def bigMultMod(a: BigInt, b: BigInt, modulus: BigInt) = {
    require(modulus.toString(2).tail.forall(_ == '0')) // modulus should be a power of the base(2)
    BigInt((a * b).toString(2).takeRight(modulus.toString(2).size - 1))
  }

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
          else BigInt("1" + solution.toString(2).padToRight(exp, '0'), 2) // put a 1 to the left

        }
        //        println(s"lifted: $liftedSolution")
        lift(liftedSolution, exp + 1)
      }
    }

    -lift(init, 1)
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
        val det = 2 * value - N
        if (det > 0) det else det + N
      }

      if (exp == 2 * lN) cal(value)
      else iter(cal(value), exp + 1)
    }

    // version 1,2
    //    iter(1, 1)

    // version 3: as modulus N has the same width as Rho(lN), iteration could start from (1 << (lN - 1))
    iter(BigInt(1) << (lN - 1), lN)
  }

  def montRed(t: BigInt, N: BigInt) = {
    require(t >= 0 && t <= N * ((BigInt(1) << lN) - 1))
    val U = bigMultMod(t, getOmega(N), Rho)
    val ret = (t + bigMult(U, N)) >> lN // divided by Rho
    val det = ret - N
    if (det > 0) det else ret
  }

  def montMul = {

  }
}

object RSAAlgo {
  def main(args: Array[String]): Unit = {

    // test for getOmega
    val algo = new RSAAlgo(512)
    val ref = new RSARef(512)
    val Zrho = Zp(asBigInteger(BigInt(1) << 512))

    def assertBig(a: BigInt, b: IntZ) = assert(a.toString(2) == b.toString(2))

    (0 until 10).foreach { _ =>
      val modulus = ref.getModulus
      //      println(s"the size of modulus is always ${modulus.toString(2).size}")
      val omega = algo.getOmega(modulus)
      assert(-omega * modulus % (BigInt(1) << 512) == 1)
      val goldenOmega: IntZ = Zrho.reciprocal(modulus)
      assertBig(-omega, goldenOmega)
      ref.refresh()
    }
    printlnGreen(s"getOmega, passed")

    (0 until 10).foreach { _ =>
      val modulus = ref.getModulus
      val ZN = Zp(modulus)
      assertBig(algo.getRhoSquare(modulus), ZN(BigInt(1) << 1024))
    }
    printlnGreen(s"getRhoSquare, passed")

    //    (0 until 1).foreach { _ =>
    //      val modulus = ref.getModulus
    //      val ZN = Zp(modulus)
    //      val input = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
    //      val RhoInverse = ZN.reciprocal(algo.Rho)
    //
    //      println(algo.montRed())
    //
    //      //      assertBig(algo.montRed(input, modulus), ZN.multiply(input, RhoInverse))
    //    }
  }
}

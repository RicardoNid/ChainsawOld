package Chainsaw.Crypto.RSA

import Chainsaw.{DSPRand, printlnGreen}
import cc.redberry.rings.scaladsl.{IntZ, Zp, asBigInteger}
import org.scalatest.funsuite.AnyFunSuite

class RSAAlgoTest extends AnyFunSuite {

  val testSize = 512
  val algo = new RSAAlgo(testSize)
  val ref = new RSARef(testSize)
  val Zrho = Zp(asBigInteger(BigInt(1) << testSize))

  def assertBig(a: BigInt, b: IntZ) = assert(a.toString(2) == b.toString(2),
    s"\nyours:  ${a.toString(2)}, \ngolden: ${b.toString(2)}")

  test("testMontRed") {
    (0 until 1000).foreach { _ =>
      val modulus = ref.getModulus
      val ZN = Zp(modulus)
      val input = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
      val RhoInverse = ZN.reciprocal(algo.Rho)
      assertBig(algo.montRed(input, modulus), ZN.multiply(input, RhoInverse))
      ref.refresh()
    }
    printlnGreen(s"montRed, passed")
  }

  test("testGetRhoSquare") {
    (0 until 1000).foreach { _ =>
      val modulus = ref.getModulus
      val ZN = Zp(modulus)
      assertBig(algo.getRhoSquare(modulus), ZN(BigInt(1) << 1024))
      ref.refresh()
    }
    printlnGreen(s"getRhoSquare, passed")
  }

  test("testGetOmega") {
    (0 until 1000).foreach { _ =>
      val modulus = ref.getModulus
      //      println(s"the size of modulus is always ${modulus.toString(2).size}")
      val omega = algo.getOmega(modulus)
      val goldenOmega: IntZ = Zrho.reciprocal(modulus)
      assertBig(algo.bigMultMod(omega, modulus, algo.Rho), algo.Rho - 1)
      assertBig(-omega + algo.Rho, goldenOmega)
      ref.refresh()
    }
    printlnGreen(s"getOmega, passed")
  }

  test("testMontExp") {
    (0 until 100).foreach { _ =>
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

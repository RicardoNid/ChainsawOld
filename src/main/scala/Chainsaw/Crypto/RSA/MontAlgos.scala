package Chainsaw.Crypto.RSA

import cc.redberry.rings.scaladsl._
import scala.math._
import Chainsaw._

import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer

object MontAlgos {
  def R2MM(X: BigInt, Y: BigInt, M: BigInt) = {
    require(X < M && Y < M)
    var S = BigInt(0)
    val n = M.bitLength + 2 // for RSA512, n = 514
    val y0 = Y.toString(2).last.asDigit
    (0 until n).foreach { i =>
      val s0 = S.toString(2).last.asDigit
      val xi = X.toString(2).padToLeft(n, '0').reverse(i).asDigit
      val qi = (xi * y0) ^ s0
      S = (S + xi * Y + qi * M) / 2
    }
    //    if (S >= M) S - M else S
    S
  }

  // TODO: implement bit select implicit
  def toWords(value: BigInt, w: Int, e: Int) = {
    value.toString(2).padToLeft(e * w, '0')
      .grouped(w).toArray.map(BigInt(_, 2))
      .reverse
  }

  // Modifications
  // - all the index of C is 1 less than the original representation, as C(0) didn't appear at all in the original representation - no we don't
  // - we add C(0) in 2.4 for consistency of A and B
  // - in the j-loop, when j = e, no addition really happens, so it is treated specially
  def MWR2MM(X: BigInt, Y: BigInt, M: BigInt, w: Int) = {
    require(X < M && Y < M)
    val n = M.bitLength + 2
    val e = ceil((n + 1).toDouble / w).toInt
    val MWords = toWords(M, w, e) :+ BigInt(0) // e + 1 elements in total
    val YWords = toWords(Y, w, e) :+ BigInt(0)
    val y0 = Y.toString(2).last.asDigit
    val SWords = ArrayBuffer.fill(e + 1)(BigInt(0))
    val CWords = ArrayBuffer.fill(e + 2)(BigInt(0)) // carry, 2-bits long
    (0 until n).foreach { i =>
      val s0 = SWords(0).toString(2).last.asDigit
      val xi = X.toString(2).padToLeft(n, '0').reverse(i).asDigit
      val qi = (xi * y0) ^ s0
      (0 to e).foreach { j =>
        val temp =
          if (j == e) CWords(j) // j = e, no addition
          else CWords(j) + xi * YWords(j) + qi * MWords(j) + SWords(j)
        CWords(j + 1) = temp / (1 << w)
        SWords(j) = temp % (1 << w)
        // j = 0, no shift
        if (j >= 1) SWords(j - 1) = BigInt(SWords(j).toString(2).last + SWords(j - 1).toString(2).padToLeft(w, '0').take(w - 1), 2)
        println(s"SWords: ${SWords.reverse.mkString(" ")}")
      }
      SWords(e) = 0
    }
    println(SWords.mkString(" "))
    val S = BigInt(SWords.take(e).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString(""), 2)
    S
  }

  def Arch1MM(X: BigInt, Y: BigInt, M: BigInt, w: Int, print: Boolean = false) = {
    val n = M.bitLength + 2
    val e = ceil((n + 1).toDouble / w).toInt
    val MWords = toWords(M, w, e)
    val YWords = toWords(Y, w, e)
    val y0 = Y.toString(2).last.asDigit
    val qs = ArrayBuffer.fill(n)(0)

    val SWords = ArrayBuffer.fill(e + 1)(BigInt(0))
    val CWords = ArrayBuffer.fill(e + 1)(BigInt(0)) // carry, 2-bits long

    (0 until e + n - 1).foreach { cycle =>
      val peFirst = if (cycle - (e - 1) <= 0) 0 else cycle - (e - 1)
      val peLast = if (cycle - (n - 1) <= 0) cycle else n - 1
      //      if (print) println(s"cycle: $cycle, workings PEs: ${(peFirst to peLast).mkString(" ")}, js: ${(peFirst to peLast).map(cycle - _).mkString(" ")}")
      (peFirst to peLast).foreach { pe =>
        val j = cycle - pe
        val i = pe
        val xi = X.toString(2).padToLeft(n, '0').reverse(i).asDigit

        if (j == 0) { // task D extra work
          val s1 = SWords(0).toString(2).padToLeft(w, '0').reverse(1).asDigit
          qs(i) = (xi * y0) ^ s1
        }
        // task D & E cowork
        val qi = qs(i)
        val YWord = YWords(j)
        val SWord = SWords(j)
        val MWord = MWords(j)
        val CWord = CWords(j)

        val tempOdd = (BigInt(1) << (w - 1)) + (SWords(j) >> 1) + CWords(j) + xi * YWords(j) + qi * MWords(j)
        val tempEven = (SWords(j) >> 1) + CWords(j) + xi * YWords(j) + qi * MWords(j)
        val det = SWords(j + 1).toString(2).last.asDigit
        CWords(j + 1) = if (det == 1) tempOdd / (BigInt(1) << w) else tempEven / (BigInt(1) << w)
        SWords(j) = if (det == 1) tempOdd % (BigInt(1) << w) else tempEven % (BigInt(1) << w)
        if (print && cycle == e + n - 2) println(s"SWords: ${SWords.mkString(" ")}")
        if (print && cycle == e + n - 2) println(s"SWords: ${SWords.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
        //        if (print && pe == peLast) println(s"SWords: ${SWords.map(_.toString(16).padToLeft(w/4, '0')).mkString(" ")}")
      }
    }

    val SBinary = SWords.take(e).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
    val S = BigInt(SBinary, 2) >> 1
    S
  }

  val ref = new RSARef(512)

  def verifyMM(algo: (BigInt, BigInt, BigInt) => BigInt) = {
    (0 until 10).foreach { _ =>
      val modulus = BigInt(ref.getModulus)
      val input0 = (modulus - DSPRand.nextInt(10000))
      val input1 = modulus - DSPRand.nextInt(10000)
      val ZN = Zp(modulus)
      val Rho = BigInt(1) << (modulus.bitLength + 2)
      val RhoInverse = ZN.reciprocal(Rho)

      val algoResult = algo(input0, input1, modulus)
      val RingsResult = BigInt(ZN.multiply(input0, input1, RhoInverse).toByteArray)
      assert(algoResult < (modulus << 1))
      assert(ZN(algoResult) == ZN(RingsResult))
    }
    printlnGreen(s"montMul, passed")
  }

  def main(args: Array[String]): Unit = {
    //        algo.verifyMM(algo.R2MM)
    //    algo.verifyMM(algo.MWR2MM(_, _, _, 4))
    MontAlgos.verifyMM(MontAlgos.Arch1MM(_, _, _, 4))
    MontAlgos.verifyMM(MontAlgos.Arch1MM(_, _, _, 16))
    MontAlgos.verifyMM(MontAlgos.Arch1MM(_, _, _, 32))
    MontAlgos.verifyMM(MontAlgos.Arch1MM(_, _, _, 64))

    def checkstyleMontMul(X: BigInt, Y: BigInt, M: BigInt): BigInt = {
      val ZN = Zp(M)
      val RhoInverse = ZN.reciprocal(BigInt(1) << M.bitLength)
      BigInt(ZN.multiply(X, Y, RhoInverse).toByteArray)
    }

    // step-by-step simulation for circuit
    println(MontAlgos.Arch1MM(BigInt(159), BigInt(148), 177, 4, print = true))
    println(MontAlgos.Arch1MM(BigInt(153), BigInt(147), 177, 4, print = true))
    println((checkstyleMontMul(159, 148, 177) << 1).toString(16))
    println((checkstyleMontMul(153, 147, 177) << 1).toString(16))
  }
}
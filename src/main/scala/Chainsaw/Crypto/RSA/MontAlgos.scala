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

  // r^2 \pmod M
  def R2MMP(M: BigInt): BigInt = {
    var minusCount = 0
    val n = M.bitLength
    var S = BigInt(1) << (M.bitLength - 1)
    println(S)
    (0 until n + 5).foreach { i =>
      S = S << 1
      S = if (S - M >= 0) {
        minusCount += 1
        S - M
      } else S
      println(S)
    }
    println(s"minus upper bound = ${n + 5}, total minus = $minusCount")
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
  def MWR2MM(X: BigInt, Y: BigInt, M: BigInt, w: Int, print: Boolean = false) = {
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
      // this is hidden as CWords(0) never modified later
      // and this make it confusing: where is C from?
      CWords(0) = BigInt(0)
      (0 to e).foreach { j =>
        val temp =
          if (j == e) CWords(j) // j = e, no addition, as input words are 0
          else CWords(j) + xi * YWords(j) + qi * MWords(j) + SWords(j)
        val SBefore = SWords(j)
        CWords(j + 1) = temp / (1 << w)
        SWords(j) = temp % (1 << w)
        // j = 0, no shift
        if (j >= 1) SWords(j - 1) = BigInt(SWords(j).toString(2).last + SWords(j - 1).toString(2).padToLeft(w, '0').take(w - 1), 2)
        if (print) println(s"xi = $xi, s0 = $s0, S = $SBefore, Y = ${YWords(j)}, M = ${MWords(j)}, C = ${CWords(j)}\nSWords: ${SWords.reverse.mkString(" ")}")
      }
      SWords(e) = 0
    }
    println(SWords.mkString(" "))
    val S = BigInt(SWords.take(e).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString(""), 2)
    S
  }

  // assignment-free form of MWR2MM
  def MWR2MMAF(X: BigInt, Y: BigInt, M: BigInt, w: Int, print: Boolean = false) = {
    require(X < M && Y < M)
    val n = M.bitLength + 2
    val e = ceil((n + 1).toDouble / w).toInt
    // use random value here as they should be irrelevant to the result
    // the input happens latter
    // TODO: change it, use ArrayBuffers that start from nothing
    val MWords = Array.fill(n + 1, e + 1)(BigInt(DSPRand.nextInt(100)))
    val YWords = Array.fill(n + 1, e + 1)(BigInt(DSPRand.nextInt(100)))
    val SWords = ArrayBuffer.fill(n + 1, e + 1)(BigInt(DSPRand.nextInt(100)))
    val CWords = ArrayBuffer.fill(n + 1, e + 2)(BigInt(DSPRand.nextInt(100))) // carry, 2-bits long
    val XBits = X.toString(2).padToLeft(n, '0').reverse
    val y0 = Y.toString(2).last.asDigit

    // input operation
    for (i <- 0 to n; j <- 0 to e) {
      MWords(0)(j) = (toWords(M, w, e) :+ BigInt(0)).apply(j) // input operation
      YWords(0)(j) = (toWords(Y, w, e) :+ BigInt(0)).apply(j) // input operation
      SWords(0)(j) = BigInt(0)
      CWords(i)(0) = BigInt(0)
    }

    for (i <- 1 to n; j <- 0 to e) { // iteration vector = (i,j)
      // control
      val s0 = SWords(i - 1)(0).toString(2).last.asDigit
      val xi = XBits(i - 1).asDigit
      val qi = (xi * y0) ^ s0

      // forwarding
      YWords(i)(j) = YWords(i - 1)(j) // expand YWords(j)
      //      println(s"$i, $j, YArray \n${YWords.map(_.mkString(" ")).mkString("\n")}")
      MWords(i)(j) = MWords(i - 1)(j)
      //      println(s"$i, $j, MArray \n${MWords.map(_.mkString(" ")).mkString("\n")}")

      // TODO: explain why C does not need a dimension expansion
      //  C has a different data structure, since its input
      //  the fact is that, C had an i index already(since it is initialized through i-axis)
      val temp = CWords(i)(j) + xi * YWords(i - 1)(j) + qi * MWords(i - 1)(j) + SWords(i - 1)(j)
      CWords(i)(j + 1) = temp / (1 << w)
      SWords(i)(j) = temp % (1 << w)

      // j = 0, no shift
      // this is special
      if (j >= 1) SWords(i)(j - 1) = BigInt(SWords(i)(j).toString(2).last + SWords(i)(j - 1).toString(2).padToLeft(w, '0').take(w - 1), 2)
      if (j == e) SWords(i)(j) = 0
      if (print) println(s"$i, $j, xi = $xi, s0 = $s0, S = ${SWords(i - 1)(j)}, Y = ${YWords(i - 1)(j)}, M = ${MWords(i - 1)(j)} C = ${CWords(i - 1)(j)} \n SArray \n${SWords.map(_.reverse.mkString(" ")).mkString("\n")}")
    }

    // output operation
    val S = BigInt(SWords.last.take(e).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString(""), 2)
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
        if (print && cycle == e + n - 2) println(s"SWords: ${SWords.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
        //        if (print && pe == peLast) println(s"SWords: ${SWords.map(_.toString(16).padToLeft(w/4, '0')).mkString(" ")}")
      }
    }

    val SBinary = SWords.take(e).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
    val S = BigInt(SBinary, 2) >> 1
    S
  }

  def Arch1ME(X: BigInt, exponent: BigInt, M: BigInt, w: Int, print: Boolean = false) = {

    val r = BigInt(Zp(M)(BigInt(1) << (M.bitLength + 2)).toByteArray)
    val rSquare = BigInt(Zp(M)(r * r).toByteArray)

    var partialProduct = X
    var montX = BigInt(1)
    // precompute
    def MM: (BigInt, BigInt) => BigInt = Arch1MM(_, _, M, w, false)
    def printTrace() = {
      if (print) println(s"partial product = ${toWordsHexString(partialProduct, w, M.bitLength / w + 1)}")
    }
    printlnGreen("before pre")
    printTrace()
    // pre, x -> x'
    val temp = MM(partialProduct, rSquare)
    partialProduct = temp
    montX = temp
    printlnGreen("after pre")
    printTrace()
    // L2R, exponent
    var count = 0
    printlnGreen("start power")
    exponent.toString(2).tail.foreach { bit =>
      println(s"bit = $bit")
      partialProduct = MM(partialProduct, partialProduct)
      if (bit.asDigit == 1) partialProduct = MM(partialProduct, montX)
      printTrace()
    }
    // post, x^e' -> x^e
    partialProduct = MM(partialProduct, BigInt(1))
    printlnGreen("after post")
    printTrace()
    // final reduction
    printlnGreen("after reduction")
    val ret = if (partialProduct >= M) partialProduct - M else partialProduct
    ret
  }

  val ref = new RSARef(512)

  // modular multiplication
  def verifyMM(algo: (BigInt, BigInt, BigInt) => BigInt) = {
    (0 until 10).foreach { _ =>
      val modulus = BigInt(ref.getModulus)
      val input0 = (modulus - DSPRand.nextInt(10000))
      val input1 = modulus - DSPRand.nextInt(10000)
      val ZN = Zp(modulus)
      val r = BigInt(1) << (modulus.bitLength + 2)
      val rInverse = ZN.reciprocal(r)

      val algoResult = algo(input0, input1, modulus)
      val RingsResult = BigInt(ZN.multiply(input0, input1, rInverse).toByteArray)
      assert(algoResult < (modulus << 1))
      println(s"golden: ${ZN(RingsResult)}")
      println(s"yours : ${ZN(algoResult)}")
      assert(ZN(algoResult) == ZN(RingsResult))
    }
    printlnGreen(s"montMul, passed")
  }

  // pre-computation(r square mod M) for MM
  def verifyMMP(algo: BigInt => BigInt) = {
    (0 until 10).foreach { _ =>
      val modulus = BigInt(ref.getModulus)
      val ZN = Zp(modulus)
      val r = BigInt(1) << (modulus.bitLength + 2)
      val algoResult = algo(modulus)
      val RingsResult = BigInt(ZN.multiply(r, r).toByteArray)
      assert(algoResult < modulus)
      assert(ZN(algoResult) == ZN(RingsResult))
    }
    printlnGreen(s"montMulPre, passed")
  }

  // modular exponentiation
  def verifyME(algo: (BigInt, BigInt, BigInt) => BigInt) = {
    (0 until 1).foreach { _ =>
      val modulus = BigInt(ref.getModulus)
      val x = modulus - DSPRand.nextInt(10000)
      val e = ref.getPublicValue
      val ZN = Zp(modulus)
      val algoResult = algo(x, e, modulus)
      val RingsResult = BigInt(ZN.pow(x, e).toByteArray)
      assert(algoResult < modulus)
      println(s"x      = ${toWordsHexString(x, 32, modulus.bitLength / 32 + 1)}")
      println(s"yours  = ${toWordsHexString(algoResult, 32, modulus.bitLength / 32 + 1)}")
      println(s"golden = ${toWordsHexString(RingsResult, 32, modulus.bitLength / 32 + 1)}")
      assert(ZN(algoResult) == ZN(RingsResult))
      //      assert(algoResult == RingsResult)
    }
    printlnGreen(s"montExp, passed")
  }

  def main(args: Array[String]): Unit = {
    //        algo.verifyMM(algo.R2MM)
    //    algo.verifyMM(algo.MWR2MM(_, _, _, 4))
    //    verifyMM(Arch1MM(_, _, _, 4))
    //    verifyMM(Arch1MM(_, _, _, 16))
    //    verifyMM(Arch1MM(_, _, _, 32))
    //    verifyMM(Arch1MM(_, _, _, 64))
    //    verifyMMP(R2MMP)
    verifyME(Arch1ME(_, _, _, 32, true))
    //    R2MMP(13)

    def checkstyleMontMul(X: BigInt, Y: BigInt, M: BigInt): BigInt = {
      val ZN = Zp(M)
      val RhoInverse = ZN.reciprocal(BigInt(1) << M.bitLength)
      BigInt(ZN.multiply(X, Y, RhoInverse).toByteArray)
    }

    // step-by-step simulation for circuit
    //        println(MontAlgos.Arch1MM(BigInt(159), BigInt(148), 177, 4, print = true))
    println(s"yours:  ${MontAlgos.MWR2MMAF(BigInt(159), BigInt(148), 177, 4)}")
    println(s"ref  :  ${MontAlgos.MWR2MM(BigInt(159), BigInt(148), 177, 4)}")
    //    println(MontAlgos.Arch1MM(BigInt(153), BigInt(147), 177, 4, print = true))
    //    println((checkstyleMontMul(159, 148, 177) << 1).toString(16))
    //    println((checkstyleMontMul(153, 147, 177) << 1).toString(16))
    //    verifyMM(MWR2MMAF(_, _, _, 4))
  }
}
package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._

import scala.math.{max, min}

class BigAlgo {

  implicit class binaryString(s: String) {
    def toBigInt = BigInt(s, 2)
    def takeLower = s.take(s.length / 2)
    def takeHigher = s.takeRight(s.length / 2)
  }

  def bigMult(a: BigInt, b: BigInt, expandedWidth: Int, baseWidth: Int) = {
    val expansionFactor = expandedWidth / baseWidth
    val aBS = a.toString(2).padToLeft(expandedWidth, '0')
    val bBS = b.toString(2).padToLeft(expandedWidth, '0')
    println(aBS, bBS)

    var partialSumReg = BigInt(0)
    var outputReg = BigInt(0)
    for (sum <- 0 until expansionFactor * 2 - 1) {
      for (i <- max(0, sum - (expansionFactor - 1)) until min(expansionFactor, sum + 1)) {
        val j = sum - i
        println(s"$i, $j")
        val multABS = aBS.reverse.slice(i * baseWidth, (i + 1) * baseWidth).reverse
        val multBBS = bBS.reverse.slice(j * baseWidth, (j + 1) * baseWidth).reverse
        val multA = multABS.toBigInt
        val multB = multBBS.toBigInt
        val multRet = multA * multB
        partialSumReg += multRet
      }
      outputReg += (partialSumReg % (1 << baseWidth)) << (sum * baseWidth) // in fact, it is concat
      partialSumReg = partialSumReg >> baseWidth
      if (sum == expansionFactor * 2 - 2) outputReg += partialSumReg << ((sum + 1) * baseWidth)
      //      println(outputReg.toString(16))
    }
    outputReg
  }

  def randomBigInt(size: Int): BigInt = BigInt((0 until size).map(_ => DSPRand.nextInt(2)).mkString(""), 2)
}

object BigAlgo {
  def main(args: Array[String]): Unit = {
    val bigAlgo = new BigAlgo
    import bigAlgo._
    val base = 8
    val expanded = 32
    val a = randomBigInt(expanded)
    val b = randomBigInt(expanded)
    assert(bigMult(a, b, expanded, base) == a * b)
  }
}

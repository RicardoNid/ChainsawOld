package Chainsaw.crypto.ReedSolomon

import cc.redberry.rings
import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class RS(n: Int, k: Int) {
  def getGxCoff = {
    val gf   = new GaloisField(n - k)
    val coff = Array.fill(n - k + 1)(0)
    coff(0) = 1
    Range(0, n - k).foreach { i =>
      val shift = Array.fill(n - k + 1)(0)
      Range(0, n - k).foreach(k => shift(k + 1) = coff(k))
      val multi = coff.map { c =>
        val mGF = gf.getPoly(BigInt(2).pow(i).toInt)
        val cGF = gf.getPoly(c)
        gf.getInt(mGF * cGF)
      }
      Range(0, n - k + 1).foreach(i => coff(i) = shift(i) ^ multi(i))
    }
    coff.reverse
  }
}

/**
 * convert between Polynomial: String and integer: BigInt
 *
 * toGFBigInt: "1+x+x^2^" -> 7
 *
 * toPolyString: 7 -> "1+x+x^2^"
 */
object PolynomialString {
  implicit class GetBigInt(s: String) {
    def toGFBigInt = {
      var ret = BigInt(0)
      if (s.contains("1"))
        ret = ret.setBit(0)
      var temp = s
      while (temp.contains("x")) {
        val i      = temp.indexOf("x")
        val length = temp.length
        if (i + 1 < length && temp(i + 1) == '^') {
          ret = ret.setBit(temp(i + 2).toString.toInt)
        } else {
          ret = ret.setBit(1)
        }
        temp = temp.substring(i + 1)
      }
      ret
    }
  }

  implicit class GetString(b: BigInt) {
    val binaryString = b.toString(2)
    def toPolyString = {
      var ret = ""
      if (b == 0)
        ret = "0"
      else {
        binaryString.toCharArray.reverse.zipWithIndex.foreach { case (c, i) =>
          if (c == '1') {
            if (i == 0) {
              ret = ret + "1"
            } else if (i == 1) {
              ret = ret + "+x"
            } else {
              ret = ret + s"+x^${i}"
            }
          }
        }
        if (ret.head == '+')
          ret = ret.tail
      }
      ret
    }
  }
}

/**
 * @param m -> GF(2^m^)
 */
case class GaloisField(m: Int = 4) {
  import PolynomialString._
  implicit val gf = GF(2, m, "x")

  def getPoly(i: Int)                     = gf(BigInt(i).toPolyString)
  def getInt(p: UnivariatePolynomialZp64) = gf(p).toString().toGFBigInt.toInt
  def alphaPowTable = {
    val table = Array.fill((1 << m) - 1)(1)
    Range(1, (1 << m) - 1).foreach { i =>
      val last  = gf(BigInt(table(i - 1)).toPolyString)
      val multi = gf(BigInt(2).toPolyString)
      table(i) = (last * multi).toString.toGFBigInt.toInt
    }
    table
  }
  def alphaIndexTable = {
    val table = Array.fill((1 << m))(0)
    alphaPowTable.zipWithIndex.foreach { case (a, i) => table(a) = i }
    table
  }

}

object GaloisFieldHardWare {

  implicit class GFUIntOp(u: UInt) {
    val m               = u.getBitsWidth
    val gf              = GaloisField(m)
    def add(that: UInt) = u ^ that
    def multi(that: Int) = {
      require(that < (1 << m) && that >= 0, "Out of Range!")
      if (that == 0)
        U(0, m bits)
      else if (that == 1)
        u
      else {
        val bufferArr = Array.fill(2 * m - 1)(ArrayBuffer[Int]())
        bufferArr.zipWithIndex.foreach { case (b, i) =>
          if (i < m)
            Range(0, i + 1).foreach(k => if (BigInt(that).testBit(i - k)) b.append(k))
          else
            Range(i - m + 1, m).foreach(k => if (BigInt(that).testBit(i - k)) b.append(k))
        }
        Range(m, 2 * m - 1).foreach { i =>
          val converted = BigInt(gf.alphaPowTable(i))
          Range(0, m).foreach { b =>
            if (converted.testBit(b)) {
              bufferArr(i).foreach(bi => if (bufferArr(b).contains(bi)) bufferArr(b).remove(bufferArr(b).indexOf(bi)) else bufferArr(b).append(bi))
            }
          }
        }
        Vec(bufferArr.take(m).map(b => b.map(u(_)).reduce(_ ^ _))).asBits.asUInt
      }
    }
  }
}


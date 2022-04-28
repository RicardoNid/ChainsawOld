package Chainsaw.crypto.ReedSolomon

import cc.redberry.rings
import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

import spinal.core._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

case class RS(n: Int, k: Int) {
  val gf        = new GaloisField(n - k)
  def getGxCoff = Range(0, n - k).map(i => Seq(1, 1 << i)).reduce((l, r) => gf.seqMulti(l, r))

  def encode(messageWord: Seq[Int]) = {
    require(messageWord.size == k)
    val mk = gf.seqMulti(messageWord, Seq(1) ++ Seq.fill(n - k)(0))
    val c  = gf.mod(mk, getGxCoff)
    messageWord ++ c
  }

  def errorLocation(coff: Seq[Int]) = {
    val end = (1 << (n - k)) - 1
    Range(0, end)
      .map(i => coff.reverse.zipWithIndex.map { case (c, k) => gf.multi(gf.alphaPowTable((k * i) % end), c) }.reduce((l, r) => l ^ r))
      .toSeq
  }
}

/** convert between Polynomial: String and integer: BigInt
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

/** @param m
  * -> GF(2^m^)
  */
case class GaloisField(m: Int = 4) {
  import PolynomialString._
  implicit val gf = GF(2, m, "x")

  def getPoly(i: Int)                     = gf(BigInt(i).toPolyString)
  def getInt(p: UnivariatePolynomialZp64) = gf(p).toString().toGFBigInt.toInt

  /** @return
    *   the result of alpha^i^ table
    */
  def alphaPowTable = {
    val table = Array.fill((1 << m) - 1)(1)
    Range(1, (1 << m) - 1).foreach { i =>
      val last  = gf(BigInt(table(i - 1)).toPolyString)
      val multi = gf(BigInt(2).toPolyString)
      table(i) = (last * multi).toString.toGFBigInt.toInt
    }
    table
  }

  /** @return
    *   the index of element represented in the form of "alpha ^i^"
    */
  def alphaIndexTable = {
    val table = Array.fill((1 << m))(0)
    alphaPowTable.zipWithIndex.foreach { case (a, i) => table(a) = i }
    table
  }

  def getInverse(a: Int) = {
    require(a > 0 && a < (1 << m))
    if (a == 1) 1
    else {
      val i   = alphaIndexTable(a)
      val j   = (1 << m) - 1 - i
      val ret = alphaPowTable(j)
      ret
    }
  }

  def multi(i: Int, j: Int) = {
    require(i >= 0 && i < (1 << m) && j >= 0 && j < (1 << m))
    val gfi     = getPoly(i)
    val gfj     = getPoly(j)
    val product = gfi * gfj
    getInt(product)
  }

  def mod(S: Seq[Int], T: Seq[Int]): Seq[Int] = {
    if (S.size < T.size) S
    else {
      val quo       = multi(getInverse(T.head), S.head)
      val mul       = T.tail.map(t => multi(quo, t)) ++ Seq.fill(S.size - T.size)(0)
      val remainder = mul.zip(S.tail).map { case (tq, s) => tq ^ s }
      mod(remainder, T)
    }

  }

  def seqMulti(A: Seq[Int], B: Seq[Int]) = {
    val l = B.length
    val S = B.zipWithIndex.map { case (b, i) => A.map(a => multi(a, b)) ++ Seq.fill(l - i - 1)(0) }
    S.map(s => Seq.fill(S.head.length - s.length)(0) ++ s) reduce ((l, r) => l.zip(r).map { case (i, j) => i ^ j })
  }
}

object coffTGest extends App {
  val rs      = RS(15, 11)
  val coff    = rs.getGxCoff
  val message = Range(1, 12).toSeq
  val sx      = rs.encode(message)
  println(sx.mkString(" "))
}

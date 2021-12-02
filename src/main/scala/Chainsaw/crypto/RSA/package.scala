package Chainsaw.crypto

import Chainsaw._

import scala.math.BigInt

package object RSA {

  def toWords(value: BigInt, w: Int, e: Int): Array[BigInt] = {
    value.toString(2).padToLeft(e * w, '0')
      .grouped(w).toArray.takeRight(e).map(BigInt(_, 2))
      .reverse
  }

  def toWordStrings(value: BigInt, wordSize: Int, wordCount: Int): Seq[String] = {
    value.toString(2).padToLeft(wordSize * wordCount, '0')
      .grouped(wordSize).toSeq.takeRight(wordCount)
  }

  // from lsb to msb
  def toWordsHexString(value: BigInt, w: Int, e: Int): String =
    toWords(value, w, e).map(_.toString(16).padToLeft(w / 4, '0') + " ").flatten.mkString("")
}

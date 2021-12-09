package Chainsaw.crypto

import Chainsaw._

import scala.math.BigInt

package object RSA {

  def toWords(value: BigInt, wordLength: Int, wordCount: Int): Seq[BigInt] = toWordStrings(value, wordLength, wordCount).map(BigInt(_, 2)).reverse

  def toWordStrings(value: BigInt, wordLength: Int, wordCount: Int): Seq[String] = {
    value.toString(2).padToLeft(wordLength * wordCount, '0')
      .grouped(wordLength).toSeq.takeRight(wordCount)
  }

  // from lsb to msb
  def toWordsHexString(value: BigInt, w: Int, e: Int): String =
    toWords(value, w, e).map(_.toString(16).padToLeft(w / 4, '0') + " ").flatten.mkString("")
}

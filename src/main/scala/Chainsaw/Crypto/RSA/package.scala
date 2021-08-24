package Chainsaw.Crypto

import Chainsaw._

import scala.math.BigInt

package object RSA {

  def toWords(value: BigInt, w: Int, e: Int) = {
    value.toString(2).padToLeft(e * w, '0')
      .grouped(w).toArray.takeRight(e).map(BigInt(_, 2))
      .reverse
  }

  def toWordStrings(value: BigInt, wordSize: Int, wordCount: Int) = {
    value.toString(2).padToLeft(wordSize * wordCount, '0')
      .grouped(wordSize).toSeq.takeRight(wordCount)
  }

  // from lsb to msb
  def toWordsHexString(value: BigInt, w: Int, e: Int) =
    toWords(value, w, e).map(_.toString(16).padToLeft(w / 4, '0') + " ").flatten.mkString("")
}

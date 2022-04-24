package Chainsaw

import Chainsaw.dsl.field.RingOp
import spinal.core._
//import scala.language.implicitConversions
import scala.math.BigInt

package object dsl {

  type Algo[TIn, TOut] = Array[TIn] => Array[TOut]
  type Impl = Vec[Bits] => Vec[Bits]
  type MixType[T] = dsl.field.MixType[T]
  type Field[T] = dsl.field.MixType[T] with RingOp[T]
  type BaseTransform[TIn, TOut] = dsl.transform.BaseTransform[TIn, TOut]
  type PureTransform[TIn, TOut] = dsl.transform.Transform[TIn, TOut]

  case class FiniteInt(value: Int) {
    override def toString = value.toString
  }

  implicit def int2finite(value: Int) = FiniteInt(value)

  implicit def base2pure[TIn, TOut](base: BaseTransform[TIn, TOut]) = base.toPure

  implicit def base2system[TIn, TOut](base: BaseTransform[TIn, TOut]) = base.toPure.toSystem

  implicit def pure2system[TIn, TOut](pure: PureTransform[TIn, TOut]) = pure.toSystem

  /** Utils for string which you treat it as a binary number
   */
  implicit class BinaryStringUtil(s: String) {
    def padToLeft(len: Int, elem: Char = '0') = s.reverse.padTo(len, elem).reverse

    def toBigInt = BigInt(s, 2)

    def asUnsigned: BigInt = s.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (BigInt(1) << i) }.sum

    def asSigned: BigInt =
      -s.head.asDigit * (BigInt(1) << s.length - 1) + // sign bit
        s.tail.asUnsigned // other bits

  }

  implicit class Int2String(i: BigInt) {
    def toUnsigned(width: Int = -1) = {
      require(i >= 0, s"$i is negative, toUnsigned failed")
      val raw = i.toString(2)
      if (width == -1) raw
      else {
        require(raw.length <= width, s"$i is too big for $width-bit unsigned number")
        raw.padToLeft(width)
      }
    }

    def toSigned(width: Int = -1) = {
      val reverse: Char => Char = x => if (x == '0') '1' else '0'

      val rawAbs = '0' + i.abs.toString(2)
      val abs = if (width == -1) rawAbs
      else {
        require(rawAbs.length <= width, s"$i is to small/big for $width-bit signed number")
        rawAbs.padToLeft(width)
      }
      if(i >= 0) abs
      else (abs.map(reverse).asUnsigned + 1).toUnsigned(width)
    }
  }

  implicit class array2d[T](array: Array[Array[T]]) {
    def toKatex = {
      val rows = array.length
      val head = s"\\left[\\begin{array}{${"c" * rows}}"
      val last = "\\end{array}\\right]"
      val contents = array.map(row => s"${row.mkString(" & ")}").mkString(" \\\\ ")
      Seq(head, contents, last).mkString(" ")
    }
  }
}

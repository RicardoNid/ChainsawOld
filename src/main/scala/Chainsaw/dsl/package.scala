package Chainsaw

import Chainsaw.dsl.ring.RingOp
import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.reflect.ClassTag
//import scala.language.implicitConversions
import scala.math.BigInt

package object dsl {

  type Algo[TIn, TOut] = Array[TIn] => Array[TOut]
  type Op2[T] = (T, T) => T

  case class HardOp2(op: Op2[Bits], latency: Int)

  type Module = (Vec[Bits], Bool) => Vec[Bits]

  type MixType[T] = dsl.ring.MixType[T]
  type Ring[T] = dsl.ring.MixType[T] with RingOp[T]

  case class FiniteInt(value: Int) {
    override def toString = value.toString
  }

  implicit def int2finite(value: Int) = FiniteInt(value)

  implicit def base2pure[TIn, TOut](base: dsl.BaseTransform[TIn, TOut]) = base.toPure

  implicit def base2system[TIn, TOut](base: dsl.BaseTransform[TIn, TOut]) = base.toPure.toSystem

  implicit def pure2system[TIn, TOut](pure: Transform[TIn, TOut]) = pure.toSystem

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
      if (i >= 0) abs
      else (abs.map(reverse).asUnsigned + 1).toUnsigned(width)
    }
  }

  implicit class arrayUtil[T: ClassTag](array: Array[T]) {
    def divide(group: Int) = array.grouped(array.length / group).toArray

    def prevAndNext(f: ((T, T)) => Unit) = array.init.zip(array.tail).foreach(f)
  }

  implicit class array2dUtil[T](array: Array[Array[T]]) {
    def toKatex = {
      val rows = array.length
      val head = s"\\left[\\begin{array}{${"c" * rows}}"
      val last = "\\end{array}\\right]"
      val contents = array.map(row => s"${row.mkString(" & ")}").mkString(" \\\\ ")
      Seq(head, contents, last).mkString(" ")
    }
  }

  // get factors of Int
  def factors(value: Int) = (1 to value).filter(value % _ == 0).toArray

}

package Chainsaw

import Chainsaw.dsl.transform.base.Matrix
import Chainsaw.dsl.vectorspace.VectorSpace
import spinal.core._

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object dsl {

  type Algo[TIn, TOut] = Array[TIn] => Array[TOut]
  type Impl = Vec[Bits] => Vec[Bits]
  type Field[T] = dsl.field.Field[T]
  type BaseTransform[TIn, TOut] = dsl.transform.BaseTransform[TIn, TOut]
  type PureTransform[TIn, TOut] = dsl.transform.PureTransform[TIn, TOut]

  case class FiniteInt(value:Int){
    override def toString = value.toString
  }
  implicit def int2finite(value:Int) = FiniteInt(value)

  implicit def base2pure[TIn, TOut](base: BaseTransform[TIn, TOut]) = base.toPure
  implicit def base2system[TIn, TOut](base: BaseTransform[TIn, TOut]) = base.toPure.toSystem
  implicit def pure2system[TIn, TOut](pure: PureTransform[TIn, TOut]) = pure.toSystem

  implicit def int2SInt(coeff: Matrix[Int])(implicit vectorSpace: VectorSpace[SInt]) = Matrix[SInt](
    coeff.array.map(_.map(S(_))))

  implicit class vec2Matrix[T <: Data](vec: Vec[T])(implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]) {
    def toMatrix = Matrix(vec.toArray)
  }

  implicit class matrix2Vec[T <: Data](mat: Matrix[T])(implicit tag: ClassTag[T]) {
    def toVec = Vec(mat.array.transpose.flatten)
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

package Chainsaw

import Chainsaw.dsl.transform.Matrix
import Chainsaw.dsl.vectorspace.VectorSpace
import spinal.core._

import scala.reflect.ClassTag

package object dsl {

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

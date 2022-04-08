package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.reflect.ClassTag

package object dsl {

  implicit def int2SInt(coeff: Matrix[Int]) = Matrix[SInt](
    coeff.array.map(_.map(S(_))), coeff.parallel, coeff.iterative)

  implicit class vec2Matrix[T <: Data](vec: Vec[T])(implicit tag :ClassTag[T]) {
    def toMatrix = Matrix(vec.toArray)
  }

  implicit class matrix2Vec[T <: Data](mat: Matrix[T])(implicit tag :ClassTag[T]) {
    def toVec = Vec(mat.array.transpose.flatten)
  }

}

package Chainsaw

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

package object fastAlgos {

  implicit class dvUtil(dv: DenseVector[Complex]) {

    def reverse = new DenseVector(dv.toArray.reverse)

    def rotateLeft(i: Int) = {
      val (l, r) = dv.toArray.splitAt(i)
      new DenseVector(r ++ l)
    }

    def rotateRight(i: Int) = {
      val (l, r) = dv.toArray.splitAt(dv.length - i)
      new DenseVector(r ++ l)
    }

  }

}

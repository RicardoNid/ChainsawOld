package Chainsaw.dsl.transform

import Chainsaw.dsl._
import spinal.core._
import spinal.lib.Delay

import scala.annotation.tailrec
import scala.reflect.ClassTag

class MatrixImpl(array: Array[Array[String]], multH: HardOp2, addH: HardOp2) extends HardImpl {

  @tailrec
  private def reduceBalancedTree[T: ClassTag](array: Array[T], op: Op2[T]): Array[T] = {
    val n = array.length
    if (n == 1) array
    else {
      val (left, right) = array.splitAt(n / 2)
      val sums = left.zip(right).map { case (a, b) => op(a, b) }
      val remain = if (n % 2 == 1) Array(array.last) else Array.empty[T]
      reduceBalancedTree(sums ++ remain, op)
    }
  }

  override val size = (array.head.length, array.length)

  override def getImpl(spaceFold: Int, timeFold: Int) = {
    val latency = log2Up(array.head.length) * addH.latency + multH.latency
    val impl = (dataIn: (Vec[Bits], Bool)) => {

      val coeffs = array.map(_.map(B(_)))
      val afterMult = coeffs.map(row => row.zip(dataIn._1).map { case (coeff, data) => multH.op(coeff, data) })
      val ret = Vec(afterMult.map(reduceBalancedTree(_, addH.op)).map(_.head))
      (ret, Delay(dataIn._2, latency, init = False))
    }
    RawImpl(impl, latency)
  }
}

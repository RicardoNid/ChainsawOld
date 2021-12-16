package Chainsaw.algos

import Chainsaw._
import breeze.linalg._

object Multiplication {

  /** complex multiplication which use 3 mults and 5 adds(rather than 4 and 2)
   */
  @fastAlgo
  def complexMult(x: BComplex, y: BComplex) = {

    val pre = new DenseMatrix(2, 3, Array(1, 0, 0, 1, 1, 1).map(_.toDouble))
    val post = new DenseMatrix(3, 2, Array(1, 0, 1, 0, 1, -1).map(_.toDouble))
    val mid = diag(DenseVector(y.real - y.imag, y.real + y.imag, y.imag))
    val data = DenseVector(x.real, x.imag)

    val retV = pre * mid * post * data
    val ret = BComplex(retV(0), retV(1))
    assert(ret == x * y, s"yours: $ret, golden: ${x * y}")
    ret
  }

  /** in this algo, we adjust the calculation order, making it suitable for FPGA DSP implementation
   */
  @hardAlgo("complexMult")
  def complexMultByDsp(x: BComplex, y: BComplex) = {

    val i0 = -x.imag
    val i1 = -y.imag

    val e = (x.real + i0) * y.real
    val i = (y.real + y.imag) * x.real - e
    val r = (y.real + i1) * x.imag + e

    val ret = BComplex(r, i)
    println(ret, x * y)
    assert(ret == x * y)
    ret
  }


  def main(args: Array[String]): Unit = {
    complexMultByDsp(BComplex(1, 1), BComplex(3, -3))
    complexMult(BComplex(1, 1), BComplex(3, -3))
  }

}

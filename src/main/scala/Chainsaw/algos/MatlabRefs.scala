package Chainsaw.algos

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import breeze.linalg.DenseVector

object MatlabRefs {

  /** linear convolution
    */
  def conv(data: DenseVector[BComplex], kernel: DenseVector[BComplex]): DenseVector[BComplex] = {
    val ret = eng.feval[Array[MComplex]]("conv", data.toMatlab, kernel.toMatlab)
    new DenseVector(ret.map(_.toBComplex))
  }

  /** cyclic convolution
    */
  def cconv(data: DenseVector[BComplex], kernel: DenseVector[BComplex]): DenseVector[BComplex] = {
    require(data.length == kernel.length)
    val ret = eng.feval[Array[MComplex]]("cconv", data.toMatlab, kernel.toMatlab, Array(kernel.length.toDouble))
    new DenseVector(ret.map(_.toBComplex))
  }

  def dft(data: DenseVector[BComplex]): DenseVector[BComplex] = {
    val ret = eng.feval[Array[MComplex]]("fft", data.toArray.map(_.toMComplex))
    new DenseVector(ret.map(_.toBComplex))
  }

  /** by default, we set order = gray and using unit average power
    */
  def qammod(data: DenseVector[Int], modulationOrder: Int) = {
    val ret = eng.feval[Array[MComplex]]("qammod", data.toArray, Array(modulationOrder.toDouble), "gray", "UnitAveragePower", Array(true))
    new DenseVector(ret.map(_.toBComplex))
  }

  def qamdemod(data: DenseVector[BComplex], modulationOrder: Int): DenseVector[Int] = {
    //    val ret = eng.feval[Array[Double]]("qamdemod", data.toMatlab, Array(modulationOrder))
    val ret = eng.feval[Array[Double]]("qamdemod", data.toMatlab, Array(modulationOrder.toDouble), "gray", "UnitAveragePower", Array(true))
    new DenseVector(ret.map(_.toInt))
  }

  def main(args: Array[String]): Unit = {}

}

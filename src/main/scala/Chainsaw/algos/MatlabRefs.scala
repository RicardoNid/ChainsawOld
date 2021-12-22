package Chainsaw.algos

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.algos.TerminationMode._
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

  def idft(data: DenseVector[BComplex]): DenseVector[BComplex] = {
    val ret = eng.feval[Array[MComplex]]("ifft", data.toArray.map(_.toMComplex))
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

  def poly2trellis(constLen: Int, codeGen: Array[Int]) = {
    eng.feval[MStruct]("poly2trellis", Array(constLen.toDouble), codeGen.map(_.toDouble))
  }

  def vitdecHard(data: DenseVector[Int], trellis: MStruct, tblen: Int, mode: TerminationMode) = {
    val modeName = mode match {
      case TRUNCATION => "trunc"
      case TERMINATION => "term"
    }
    val ret = eng.feval[Array[Double]]("vitdec", data.toArray.map(_.toDouble), trellis, Array(tblen.toDouble), modeName, "hard")
    new DenseVector(ret.map(_.toInt))
  }

  def convenc(data: DenseVector[Int], trellis: MStruct) = {
    val ret = eng.feval[Array[Double]]("convenc", data.toArray.map(_.toDouble), trellis).map(_.toInt)
    new DenseVector(ret)
  }

  def main(args: Array[String]): Unit = {
    val trellis = poly2trellis(7, Array(171, 133))
    val dataIn = ChainsawRand.nextBits(128)
    val coded = vitdecHard(new DenseVector(dataIn.toArray), trellis, 42, TerminationMode.TRUNCATION)
    println(coded)
  }

}

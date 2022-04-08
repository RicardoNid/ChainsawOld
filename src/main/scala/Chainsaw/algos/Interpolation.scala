package Chainsaw.algos

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import breeze.stats.mean

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object Interpolation {

  type Image = Seq[DenseMatrix[Double]]

  def mse(ref: Image, recovered: Image): Double = {
    val psnrs = (0 until 3).map(i => matlabIO.eng.feval("mse", ref(i).toArray, recovered(i).toArray).asInstanceOf[Double])
    psnrs.sum / 3
  }

  def main(args: Array[String]): Unit = {
    val image: Image = Seq.fill(3)(DenseMatrix.tabulate(10, 10)((i, j) => 1))
    val imageWithNoise: Image = Seq.fill(3)(DenseMatrix.tabulate(10, 10)((i, j) => 2))

    println(mse(image, imageWithNoise))
  }

}

package Chainsaw.algos

import breeze.numerics.abs
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

object Division {

  /** division by convergence
   *
   * @param denominator the denominator must be close to 1, smaller than 2.0
   * @see ''Digital Signal Processing with Field ProgrammableGate Arrays'' Algo 2.20
   */
  @hardAlgo("division")
  def dbc(numerator: Double, denominator: Double, iteration: Int) = {
    require(denominator > 0.5 && denominator < 2)
    val xs = ArrayBuffer[Double](numerator)
    val ts = ArrayBuffer[Double](denominator)
    val ss = ArrayBuffer[Double](denominator - 1)

    (0 until iteration).foreach { _ =>
      //      val (fk, xk, tk) = (2.0 - ts.last, xs.last, ts.last)
      //      xs += xk * fk
      //      ts += tk * fk

      val (xk, tk) = (xs.last, ts.last)
      xs += xk * (2.0 - tk)
      ts += tk * (2.0 - tk)
    }

    assert(abs(numerator / denominator - xs.last) < 1E-4)
    xs.last
  }

  @hardAlgo("dbc")
  def dbcFixed(numerator: SFix, denominator: SFix, iteration: Int) = {

    val fixedType = HardType(numerator)

    val maxExp = numerator.maxExp
    val minExp = numerator.minExp

    val xs = numerator +: Seq.fill(iteration)(fixedType())
    val ts = denominator +: Seq.fill(iteration)(fixedType())

    (0 until iteration).foreach { i =>
      val (fk, xk, tk) = (SF(2.0, maxExp exp, minExp exp) - ts(i), xs(i), ts(i))
      xs(i + 1) := (xk * fk).truncated
      ts(i + 1) := (tk * fk).truncated
    }

    xs.last
  }

}

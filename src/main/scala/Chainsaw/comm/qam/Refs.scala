package Chainsaw.comm.qam

import Chainsaw.matlabIO.{MComplex, eng}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._

object Refs {

  // the default scheme of Matlab is gray
  def qammod(input: Array[Int], bitPerSymbol: Int, gray: Boolean = true): Array[MComplex] = {
    if (!gray) eng.feval[Array[MComplex]]("qammod", input, Array(1 << bitPerSymbol), "bin")
    else eng.feval[Array[MComplex]]("qammod", input, Array(1 << bitPerSymbol), "gray")
  }

  def qamdemod(input: Array[MComplex], bitPerSymbol: Int, gray: Boolean = true): Array[Int] = {
    val ret = if (!gray) eng.feval[Array[Double]]("qamdemod", input, Array(1 << bitPerSymbol), "bin")
    else eng.feval[Array[Double]]("qamdemod", input, Array(1 << bitPerSymbol), "gray")
    ret.map(_.toInt)
  }

  def qamdemod(input: MComplex, bitPerSymbol: Int, gray: Boolean) = {
    val ret = if (!gray) eng.feval[Double]("qamdemod", input, Array(1 << bitPerSymbol), "bin")
    else eng.feval[Double]("qamdemod", input, Array(1 << bitPerSymbol), "gray")
    ret.toInt
  }

  def getQAMValues(bits: Int, gray: Boolean = true) = {
    require(bits >= 1)
    val M = 1 << bits
    val values = (0 until M).toArray
    if (bits == 1) Array(MComplex(-1, 0), MComplex(1, 0)) else { // caution: matlab default = gray
      if (gray) eng.feval[Array[MComplex]]("qammod", values, Array(M), "gray")
      else eng.feval[Array[MComplex]]("qammod", values, Array(M), "bin")
    }
  }

  def getQAMRms(bits: Int) = eng.feval[Double]("rms", getQAMValues(bits))

  def main(args: Array[String]): Unit = { // explore the threshold of QAMDeMod
    val inputs = (0 until 1000).map(_ => DSPRand.nextComplex(-3, 3)).toArray
    val demapped = qamdemod(inputs, 3)
    eng.putVariable("inputs", inputs)
    eng.putVariable("demapped", demapped)
    eng.eval("plot(inputs(demapped == 0), 'c.'); " +
      "hold on;" +
      "plot(inputs(demapped == 1), 'g.')")
  }
}

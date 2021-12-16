package Chainsaw.comm.channelEqualizer

import Chainsaw.{BComplex, matlabIO}
import breeze.linalg.{DenseMatrix, DenseVector, max}
import breeze.numerics.abs
import breeze.stats.mean
import spinal.core.sim.SimConfig
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

object FreqEqualizerAlgo {

  def main(args: Array[String]): Unit = {

    import matlabIO._

    val dataFromMatlab = eng.load("~/FTN326/reference/ChanEquCoreInput.mat", "ChanEquCoreInput").asInstanceOf[Array[Array[MComplex]]]
    val dataIn = new DenseMatrix(2, 256, dataFromMatlab.flatten.map(_.toBComplex)) / BComplex(256.0, 0)

    val factorsFromMatlab = eng.load("~/FTN326/reference/ChanEquCoreOutput.mat", "ChanEquCoreOutput").asInstanceOf[Array[MComplex]]
    val factors = new DenseVector(factorsFromMatlab.map(_.toBComplex)) / BComplex(1024.0, 0)

    val goldenFromMatlab = eng.load("~/FTN326/reference/PreambleMappedSymbols", "PreambleMappedSymbols").asInstanceOf[Array[Double]]
    val golden = new DenseVector(goldenFromMatlab)

    println(s"golden factors: \n$factors")

    val preamble: Seq[DenseVector[BComplex]] = (0 until 2).map(i => dataIn(i, ::).t)
    println(golden)
    println(s"preamble0: \n${preamble(0)}")
    println(s"preamble1: \n${preamble(1)}")

    val data: Seq[DenseVector[BComplex]] = (2 until 18).map(i => dataIn(i % 2, ::).t)

    val vZero = DenseVector.zeros[Double](256)

    def add(x: DenseVector[Double], y: DenseVector[Double]) = x + y

    def sub(x: DenseVector[Double], y: DenseVector[Double], z: DenseVector[Double]) = x - y

    def mult(x: DenseVector[Double], y: DenseVector[Double]) = x * y

    def mac(x: DenseVector[Double], y: DenseVector[Double], z: DenseVector[Double]) = x * y + z

    def linear(x: DenseVector[Double], k: Double, b: Double) = x * k + b

    def shiftLeft(x: DenseVector[Double], i: Int) = DenseVector.vertcat(x(i to -1), DenseVector.fill(i)(x(-1)))

    def shiftRight(x: DenseVector[Double], i: Int) = DenseVector.vertcat(DenseVector.fill(i)(x(0)), x(0 to -(1 + i)))


    // block1: adjustment && smooth, using adders
    var reg0, reg1, reg2, reg3 = DenseVector.zeros[Double](256)
    // input
    reg0 = preamble(0).map(_.real)
    reg1 = preamble(0).map(_.imag)
    // add
    reg0 = add(reg0, preamble(1).map(_.real))
    reg1 = add(reg1, preamble(1).map(_.imag))
    // adjustment
    reg0 = reg0 * golden * 0.5
    reg2 = DenseVector.vertcat(DenseVector.fill(7)(reg0(0)), reg0)

    reg1 = reg1 * golden * 0.5
    reg3 = DenseVector.vertcat(DenseVector.fill(7)(reg1(1)), reg1)

    // smooth
    reg0 = vZero
    reg1 = vZero
    (0 until 16).foreach { i =>
      reg0 = reg0 + reg2(0 until 256)
      reg2 = shiftLeft(reg2, 1)

      reg1 = reg1 + reg3(0 until 256)
      reg3 = shiftLeft(reg3, 1)
    }

    var reals = reg0 / 16.0
    var imags = reg1 / 16.0
    println(s"after smooth")
    val realAfterSmooth = reals(2 to 225)
    val imagsAfterSmooth = imags(2 to 225)
    println(s"reals: \n$realAfterSmooth")
    println(s"imags: \n$imagsAfterSmooth")

    // block2: energy, division & multiplication
    var xk, tk = DenseVector.ones[Double](256)
    // get energy
    tk = mult(reals, reals)
    xk = mult(imags, imags) // now, xk = 1, tk = energy
    tk = add(xk, tk)
    // init before ...
    tk = tk / 4.0
    xk = DenseVector.ones[Double](256)
    // dbc, after that, xk = energy
    (0 until 10).foreach { _ =>
      xk = xk * (2.0 - tk)
      tk = tk * (2.0 - tk)
    }
    // get factor, for the following cycles, reals and imags keep the factor
    reals = reals * xk / 4.0
    imags = -imags * xk / 4.0
    println("final")
    println(s"reals: \n$reals")
    println(s"imags: \n$imags")
    val myFactors = DenseVector.tabulate(256)(i => BComplex(reals(i), imags(i)))

    val temp = factors - myFactors
    val diff = temp(2 to 225)
    println(s"diff: \n$diff")
    println(s"diff abs mean: \n${mean(abs(diff))}")
    println(s"diff real mean: \n${mean(abs(diff.map(_.real)))}")
    println(s"diff imag mean: \n${mean(abs(diff.map(_.imag)))}")

    // use factor
    val ret = ArrayBuffer[DenseVector[Double]]()
    data.foreach { vec => // TODO: replace by complex multiplication subroutine

    }

    // verify the smooth module
    SimConfig.withWave.compile(Smooth(golden.toArray.toSeq.map(_.toInt), HardType(SFix(7 exp, 18 bits)))).doSim { dut =>

      val dutResult = ArrayBuffer[Seq[BComplex]]()
      dut.clockDomain.forkStimulus(2)
      dut.dataIn.payload.zip(preamble(0).toArray).foreach { case (port, complex) => port #= complex }
      dut.clockDomain.waitSampling()
      dut.dataIn.payload.zip(preamble(1).toArray).foreach { case (port, complex) => port #= complex }
      dut.clockDomain.waitSampling()
      (0 until 50).foreach { _ =>
        if (dut.counter.value.toInt == 41) {
          dut.clockDomain.waitSampling()
          dutResult += dut.dataOut.payload.map(_.toComplex)
        }
        dut.clockDomain.waitSampling()
      }
      val temp = new DenseVector(dutResult(0).toArray)
      val yoursAfterSmooth = temp(2 to 225)
      val goldenAfterSmooth = DenseVector.tabulate(224)(i => BComplex(realAfterSmooth(i), imagsAfterSmooth(i)))
      val diff = yoursAfterSmooth - goldenAfterSmooth
      assert(diff.forall(_.abs < 1E-1), max(abs(diff))) // for extreme value
      assert(mean(abs(diff)) < 1E-2, mean(abs(diff))) // for mean performance
    }

  }
}

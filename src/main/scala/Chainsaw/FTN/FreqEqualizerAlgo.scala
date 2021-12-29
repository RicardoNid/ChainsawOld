package Chainsaw.FTN

import Chainsaw._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.abs
import breeze.stats.mean
import spinal.core._

object FreqEqualizerAlgo {

  def add(x: DenseVector[Double], y: DenseVector[Double]) = x + y

  def mult(x: DenseVector[Double], y: DenseVector[Double]) = x * y

  def shiftLeft(x: DenseVector[Double], i: Int) = DenseVector.vertcat(x(i to -1), DenseVector.fill(i)(x(-1)))

  def loadData() = {
    import matlabIO._

    val preambleFromMatlab = eng.load("~/FTN326/reference/ChanEquCoreInput.mat", "ChanEquCoreInput").asInstanceOf[Array[Array[MComplex]]]
    val preambleIn = new DenseMatrix(2, 256, preambleFromMatlab.flatten.map(_.toBComplex))
    val preamble: Seq[DenseVector[BComplex]] = (0 until 2).map(i => preambleIn(i, ::).t)

    val dataFromMatlab = eng.load("~/FTN326/reference/DataBeforeEqualization.mat", "DataBeforeEqualization").asInstanceOf[Array[Array[MComplex]]]
    val dataIn = new DenseMatrix(16, 256, dataFromMatlab.flatten.map(_.toBComplex))
    val data: Seq[DenseVector[BComplex]] = (0 until 16).map(i => dataIn(i, ::).t)

    val factorsFromMatlab: Array[MComplex] = eng.load("~/FTN326/reference/ChanEquCoreOutput.mat", "ChanEquCoreOutput").asInstanceOf[Array[MComplex]]
    val goldenFactors = new DenseVector(factorsFromMatlab.map(_.toBComplex))

    val symbolsFromMatlab = eng.load("~/FTN326/reference/PreambleMappedSymbols", "PreambleMappedSymbols").asInstanceOf[Array[Double]]
    val goldenSymbols = new DenseVector(symbolsFromMatlab)

    val resultsFromMatlab = eng.load("~/FTN326/reference/DataAfterEqualization", "DataAfterEqualization").asInstanceOf[Array[Array[MComplex]]]
    val resultDM = new DenseMatrix(16, 256, resultsFromMatlab.flatten.map(_.toBComplex))
    val goldenResults = (0 until 16).map(i => resultDM(i, ::).t)

    (preamble, data, goldenSymbols, goldenFactors, goldenResults)
  }

  def smooth(preamble: Seq[DenseVector[BComplex]], goldenSymbols: DenseVector[Double]) = {
    val vZero = DenseVector.zeros[Double](256)

    // block1: adjustment && smooth, using adders
    var reg0, reg1, reg2, reg3 = DenseVector.zeros[Double](256)
    // input
    reg0 = preamble(0).map(_.real)
    reg1 = preamble(0).map(_.imag)
    // add
    reg0 = add(reg0, preamble(1).map(_.real))
    reg1 = add(reg1, preamble(1).map(_.imag))
    // adjustment
    reg0 = reg0 * goldenSymbols * 0.5
    reg2 = DenseVector.vertcat(DenseVector.fill(7)(reg0(0)), reg0)

    reg1 = reg1 * goldenSymbols * 0.5
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

    val reals = reg0 / 16.0
    val imags = reg1 / 16.0
    (reals, imags)
  }

  def equal0(realAfterSmooth: DenseVector[Double], imagAfterSmooth: DenseVector[Double]) = {
    var reals = realAfterSmooth
    var imags = imagAfterSmooth

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
    (reals, imags)
  }

  def equal1(factors: DenseVector[BComplex], data: Seq[DenseVector[BComplex]]) = data.map(vec => vec * factors)

  def main(args: Array[String]): Unit = {

    val (preamble, data, goldenSymbols, factors, goldenResults) = loadData()
    val (realAfterSmooth, imagAfterSmooth) = smooth(preamble, goldenSymbols)
    val (reals, imags) = equal0(realAfterSmooth, imagAfterSmooth)

    val myFactors = DenseVector.tabulate(256)(i => BComplex(reals(i), imags(i)))

    val temp = factors - myFactors
    val factorDiff = temp(2 to 225)
    val factorDiffMean = mean(abs(factorDiff))
    assert(factorDiffMean < 1E-2, s"factor diff: \n$factorDiff\nmean = $factorDiffMean")

    val myResults = equal1(myFactors, data)

    val resultDiffs = goldenResults.zip(myResults).map { case (golden, yours) => golden - yours }
    val resultDiffMean = resultDiffs.map(diff => mean(abs(diff(2 to 225))))
    resultDiffMean.foreach(mean => assert(mean < 1E-2, s"$resultDiffMean"))

  }
}

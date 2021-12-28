package Chainsaw.FTN


import Chainsaw.FTN.FreqEqualizerAlgo.{equal0, equal1, loadData, smooth}
import Chainsaw._
import Chainsaw.dspTest._
import breeze.linalg._
import breeze.numerics._
import breeze.stats.mean
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class EqualizerFTNTest extends AnyFlatSpec {

  behavior of "EqualizerFTNTest"

  val (preamble, data, goldenSymbols, goldenFactors, goldenResults) = loadData()
  val (realAfterSmooth, imagAfterSmooth) = smooth(preamble, goldenSymbols)
  val (reals, imags) = equal0(realAfterSmooth, imagAfterSmooth)
  val factors = DenseVector.tabulate(256)(i => BComplex(reals(i), imags(i)))
  val golden = equal1(factors, data)

  val symbols = (goldenSymbols.toArray.toSeq.map(_.toInt))

  it should "gen" in GenRTL(EqualizerFTN(symbols))

  it should "work" in {

    SimConfig.withWave.compile(EqualizerFTN(symbols)).doSim { dut =>

      dut.dataIn.valid #= false
      dut.dataOut.ready #= true

      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()

      def runMode0(): Unit = {
        (0 until 80).foreach { i =>
          if (i < 2) {
            dut.dataIn.valid #= true
            dut.dataIn.payload.zip(preamble(i).toArray).foreach{ case (port, complex) => port #= complex}
          } else if (i < 18) {
            dut.dataIn.valid #= true
            dut.dataIn.payload.zip(data(i - 2).toArray).foreach{ case (port, complex) => port #= complex}
          }
          else dut.dataIn.valid #= false
          dut.clockDomain.waitSampling()
        }
      }

      val dutResults = ArrayBuffer[BComplex]()
      dut.dataOut.setMonitor(dutResults)

      runMode0()
      runMode0()
      runMode0()
      dut.clockDomain.waitSampling(100)

      println(dutResults.head)
      println(goldenResults.head)
//      val groupedDutResults = dutResults.toSeq.grouped(256).toSeq.map(vec => new DenseVector(vec.toArray))
//      val resultDiffs = goldenResults.zip(groupedDutResults).map { case (golden, yours) => golden - yours }
//      val resultDiffMean = resultDiffs.map(diff => mean(abs(diff(2 to 225))))
//      resultDiffMean.foreach(mean => assert(mean < 5E-2, s"$resultDiffMean"))
    }

  }

//  it should "synth" in VivadoSynth(EqualizerFTN(symbols))
}

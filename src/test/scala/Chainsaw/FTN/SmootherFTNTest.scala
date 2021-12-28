package Chainsaw.FTN

import Chainsaw._
import breeze.linalg._
import breeze.numerics._
import breeze.stats.mean
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

import FreqEqualizerAlgo.{loadData, equal0, equal1, smooth}

class SmootherFTNTest extends AnyFlatSpec {

  behavior of "SmoothTest"

  val (preamble, data, goldenSymbols, factors, results) = loadData()
  val (realAfterSmooth, imagAfterSmooth) = smooth(preamble, goldenSymbols)

  it should "work" in {

    SimConfig.withWave.compile(SmootherFTN(goldenSymbols.toArray.toSeq.map(_.toInt))).doSim { dut =>

      val dutResult = ArrayBuffer[Seq[BComplex]]()
      dut.clockDomain.forkStimulus(2)
      dut.dataIn.valid #= true
      dut.clockDomain.waitSampling()
      dut.dataIn.payload.zip(preamble(0).toArray).foreach { case (port, complex) => port #= complex }
      dut.clockDomain.waitSampling()
      dut.dataIn.payload.zip(preamble(1).toArray).foreach { case (port, complex) => port #= complex }
      (0 until 50).foreach { _ =>
        if (dut.counter.value.toInt == dut.latency - 1) {
          dut.clockDomain.waitSampling()
          dutResult += dut.dataOut.payload.map(_.toComplex)
        }
        dut.clockDomain.waitSampling()
      }
      val yoursAfterSmooth = new DenseVector(dutResult(0).toArray)
      val goldenAfterSmooth = DenseVector.tabulate(256)(i => BComplex(realAfterSmooth(i), imagAfterSmooth(i)))
      val diff = yoursAfterSmooth - goldenAfterSmooth
      assert(diff.forall(_.abs < 1E-1), max(abs(diff))) // for extreme value
      assert(mean(abs(diff)) < 1E-2, mean(abs(diff))) // for mean performance
    }
  }

  it should "synth" in {
    VivadoSynth(SmootherFTN(goldenSymbols.toArray.toSeq.map(_.toInt)), "Smooth256")
  }

}

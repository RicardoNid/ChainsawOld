package Chainsaw.comm.channelEqualizer

import Chainsaw._
import breeze.linalg._
import breeze.numerics._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class SmoothTest extends AnyFlatSpec {

  behavior of "SmoothTest"

//  it should "work" in {
//    SimConfig.withWave.compile(Smooth(golden.toArray.toSeq.map(_.toInt), HardType(SFix(7 exp, 18 bits)), 256)).doSim { dut =>
//
//      val dutResult = ArrayBuffer[Seq[BComplex]]()
//      dut.clockDomain.forkStimulus(2)
//      dut.dataIn.payload.zip(preamble(0).toArray).foreach { case (port, complex) => port #= complex }
//      dut.clockDomain.waitSampling()
//      dut.dataIn.payload.zip(preamble(1).toArray).foreach { case (port, complex) => port #= complex }
//      dut.clockDomain.waitSampling()
//      (0 until 50).foreach { _ =>
//        if (dut.counter.value.toInt == 41) {
//          dut.clockDomain.waitSampling()
//          dutResult += dut.dataOut.payload.map(_.toComplex)
//        }
//        dut.clockDomain.waitSampling()
//      }
//      val temp = new DenseVector(dutResult(0).toArray)
//      val yoursAfterSmooth = temp(2 to 225)
//      val goldenAfterSmooth = DenseVector.tabulate(224)(i => BComplex(realAfterSmooth(i), imagsAfterSmooth(i)))
//      val diff = yoursAfterSmooth - goldenAfterSmooth
//      assert(diff.forall(_.abs < 1E-1), max(abs(diff))) // for extreme value
//      assert(mean(abs(diff)) < 1E-2, mean(abs(diff))) // for mean performance
//    }
//  }

}

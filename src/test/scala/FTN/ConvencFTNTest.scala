package FTN

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class ConvencFTNTest extends AnyFunSuite {

  test("testConvenc for FTN") {
    val config = ConvencConfig(7, Array(171, 133))
    SimConfig.withWave.compile(new ConvencFTN(config, 128)).doSim { dut =>
      import dut._
      val eng = AsyncEng.get()
      clockDomain.forkStimulus(2)

      val latency = 64
      val inputs = (0 until latency).map { _ =>
        val bytes = Array.fill(parallelFactor / 8)(1.toByte)
        DSPRand.nextBytes(bytes)
        BigInt(bytes).abs
      }

      val inputFlatten = inputs.map(_.toString(2).padToLeft(parallelFactor, '0').map(_.asDigit)).flatten
      val outputFlatten = ArrayBuffer[Int]()

      (0 until latency).foreach { i =>
        dataIn #= inputs(i)
        clockDomain.waitSampling()
      }

      (0 until latency).foreach { i =>
        clockDomain.waitSampling()
        outputFlatten ++= dataOut.toBigInt.toString(2).padToLeft(parallelFactor, '0').map(_.asDigit)
      }

      val trellis = MatlabRef.poly2trellis(dut.config.K, dut.config.codeGens)
      val golden = MatlabRef.convenc(inputFlatten.toArray.map(_.toDouble), trellis).map(_.toInt)
      val yours = outputFlatten

      println(golden.mkString(""))
      println(yours.mkString(""))
      printlnGreen("first cycle of I/O ")
      println(golden.take(parallelFactor).mkString(""))
      println(yours.take(parallelFactor).mkString(""))
    }
  }

}

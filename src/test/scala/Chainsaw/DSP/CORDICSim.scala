package Chainsaw.DSP

import Chainsaw.DSP.AlgebraicMode.{CIRCULAR, HYPERBOLIC, LINEAR}
import Chainsaw.DSP.CordicArch.PARALLEL
import Chainsaw.DSP.RotationMode.{ROTATION, VECTORING}
import Chainsaw.{DSPSimTiming, Double2Fix, Fix2Double, sameFixedSeq, _}
import breeze.numerics._
import breeze.numerics.constants.Pi
import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._

class CORDICSim(cordicConfig: CordicConfig) extends CORDICDUT(cordicConfig) with DSPSimTiming[CordicData, CordicData, CordicSimData, CordicSimData] {
  cordic.start := True

  // TODO: implement a DSPSim for handshake
  //  cordic.setStart(input.valid)

  override def poke(testCase: CordicSimData, input: CordicData): Unit = {
    input.x.raw #= Double2Fix(testCase.x, 14)
    input.y.raw #= Double2Fix(testCase.y, 14)
    input.z.raw #= Double2Fix(testCase.z, 13)
    clockDomain.waitSampling()
  }

  override def peek(output: CordicData): CordicSimData = {
    val ret = CordicSimData(Fix2Double(output.x, 14),
      Fix2Double(output.y, 14),
      Fix2Double(output.z, 13))
    clockDomain.waitSampling()
    ret
  }

  override def isValid(refResult: CordicSimData, dutResult: CordicSimData): Boolean =
    sameFixedSeq(Array(refResult.x, refResult.y, refResult.z),
      Array(dutResult.x, dutResult.y, dutResult.z))

  override def messageWhenInvalid(testCase: CordicSimData, refResult: CordicSimData, dutResult: CordicSimData): String =
    s"\n[ERROR]\ninput: $testCase\ngolden: $refResult\nyours: $dutResult"

  override def messageWhenValid(testCase: CordicSimData, refResult: CordicSimData, dutResult: CordicSimData): String = ""

  override def referenceModel(testCase: CordicSimData): CordicSimData = {
    val x = testCase.x
    val y = testCase.y
    val z = testCase.z
    cordicConfig.rotationMode match {
      case ROTATION =>
        cordicConfig.algebricMode match {
          case CIRCULAR => CordicSimData(x * cos(z) - y * sin(z), y * cos(z) + x * sin(z), 0.0)
          case HYPERBOLIC => CordicSimData(x * cosh(z) - y * sinh(z), y * cosh(z) + x * sinh(z), 0.0)
          case LINEAR => CordicSimData(x, y + x * z, 0.0)
        }
      case VECTORING =>
        cordicConfig.algebricMode match {
          case CIRCULAR => CordicSimData(sqrt(x * x + y * y), 0.0, z + atan(y / x))
          case HYPERBOLIC => CordicSimData(sqrt(x * x - y * y), 0.0, z + atanh(y / x))
          case LINEAR => CordicSimData(x, 0.0, z + y / x)
        }
    }
  }
}

class testCORDIC extends FunSuite {
  private def randomCase(cordicConfig: CordicConfig) = {
    val theta = (DSPRand.nextDouble() * 0.5 - 0.25) * Pi
    val unit = DSPRand.nextDouble() * 2 - 1
    cordicConfig.algebricMode match {
      case CIRCULAR =>
        cordicConfig.rotationMode match {
          case ROTATION => CordicSimData(1.0, 0.0, theta)
          case VECTORING =>
            val x = DSPRand.nextDouble()
            val y = unit * 0.707 * x
            CordicSimData(x, y, 0.0)
        }
      case HYPERBOLIC =>
        cordicConfig.rotationMode match {
          case ROTATION => CordicSimData(1.0, 0.0, theta)
          case VECTORING =>
            val x = DSPRand.nextDouble() * 2
            val y = unit * x * 0.8
            CordicSimData(x, y, 0.0)
        }
      case LINEAR =>
        cordicConfig.rotationMode match {
          case ROTATION => CordicSimData(1.0, 0.0, theta)
          case VECTORING =>
            val x = DSPRand.nextDouble()
            val y = unit * x * 0.707
            CordicSimData(x, y, 0.0)
        }
    }
  }

  def randomSim(cordicConfig: CordicConfig): Unit = {
    val dut = SimConfig.withWave.compile(new CORDICSim(cordicConfig))
    dut.doSim { dut =>
      dut.sim()
      for (_ <- 0 until 10000) dut.insertTestCase(randomCase(cordicConfig))
      val report = dut.simDone()
      println(Console.RED)
      println(report.log.mkString(" "))
      print(Console.GREEN)
      println(report.validLog.mkString(" "))
      println(s"[RESULT] ${report.trueCase} / ${report.totalCase} passed")
      println(s"$cordicConfig, sim done")
      print(Console.BLACK)
    }
  }

  test("testCORDIC") {
    ChainsawDebug = true
    for (algebraic <- AlgebraicMode.values; rotation <- RotationMode.values; arch <- IndexedSeq(PARALLEL)) {
      randomSim(CordicConfig(rotationMode = rotation, algebricMode = algebraic, cordicArch = arch))
    }
  }
}
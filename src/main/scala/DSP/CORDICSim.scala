package DSP

import DSP.AlgebraicMode._
import DSP.CordicArch._
import DSP.RotationMode._
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, Flow, master, slave}

import scala.util.Random

case class CordicSimData(x: Double, y: Double, z: Double) {
  override def toString: String = s"x: $x, y: $y, z: $z"
}

case class CordicData() extends Bundle {
  val x: SFix = SFix(1 exp, -14 exp) // 1QN
  val y: SFix = SFix(1 exp, -14 exp) // 1QN
  val z: SFix = SFix(2 exp, -13 exp) // 2QN
}

class CORDICSim(cordicConfig: CordicConfig) extends Component with DSPSimLatest[CordicData, CordicData, CordicSimData, CordicSimData] {
  override val input: Flow[CordicData] = slave Flow CordicData()
  override val output: Flow[CordicData] = master Flow CordicData()

  val cordic: CORDIC = CORDIC(input.payload.x, input.payload.y, input.payload.z, cordicConfig)

  output.payload.x := cordic._1.truncated
  output.payload.y := cordic._2.truncated
  output.payload.z := cordic._3.truncated
  override val timing: TimingInfo = cordic.getTimingInfo
  output.valid := Delay(input.valid, timing.latency, init = False)

  cordic.setStart(input.valid)

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

object CORDICSim {
  private def randomCase(cordicConfig: CordicConfig) = {
    val r = Random
    val theta = (r.nextDouble() * 0.5 - 0.25) * Pi
    val unit = r.nextDouble() * 2 - 1
    cordicConfig.algebricMode match {
      case CIRCULAR =>
        cordicConfig.rotationMode match {
          case ROTATION => CordicSimData(1.0, 0.0, theta)
          case VECTORING =>
            val x = r.nextDouble()
            val y = unit * 0.707 * x
            CordicSimData(x, y, 0.0)
        }
      case HYPERBOLIC =>
        cordicConfig.rotationMode match {
          case ROTATION => CordicSimData(1.0, 0.0, theta)
          case VECTORING =>
            val x = r.nextDouble() * 2
            val y = unit * x * 0.8
            CordicSimData(x, y, 0.0)
        }
      case LINEAR =>
        cordicConfig.rotationMode match {
          case ROTATION => CordicSimData(1.0, 0.0, theta)
          case VECTORING =>
            val x = r.nextDouble()
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

  def main(args: Array[String]): Unit = {
    debug = true
    for (algebraic <- AlgebraicMode.values; rotation <- RotationMode.values; arch <- IndexedSeq(SERIAL)) {
      randomSim(CordicConfig(rotationMode = rotation, algebricMode = algebraic, cordicArch = arch))
    }
  }
}





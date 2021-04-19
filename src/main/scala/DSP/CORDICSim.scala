package DSP

import DSP.AlgebricMode._
import DSP.CordicArch.{CordicArch, SERIAL}
import DSP.RotationMode._
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core.sim._

import scala.util.Random

case class cordicTestCase(x: Double, y: Double, z: Double) extends TestCase {
  override def toString: String = s"x: $x, y: $y, z: ${z / Pi * 180.0} degree"
}

class CORDICSim(rotationMode: RotationMode, algebricMode: AlgebricMode, cordicArch: CordicArch)
  extends CORDICGen(rotationMode = rotationMode, algebricMode = algebricMode, cordicArch = cordicArch)
    with DSPSim {
  override type TestCase = cordicTestCase
  override type ResultType = Array[Double]

  override def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    input.valid #= false
    clockDomain.waitSampling(10)
  }

  override def simDone(): Unit = {
    clockDomain.waitSampling(10)
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(10)
  }

  override def driver(): Unit = {
    val drv = fork {
      while (true) {
        if (testCases.nonEmpty) {
          val testCase = testCases.dequeue()
          cordicArch match {
            case DSP.CordicArch.PARALLEL => {

              //          println("test: " + testCase.toString)
              input.valid #= true
              input.payload.x.raw #= Double2Fix(testCase.x)
              input.payload.y.raw #= Double2Fix(testCase.y)
              input.payload.z.raw #= Double2Fix(testCase.z)
              clockDomain.waitSampling()
              input.valid #= false
              val refResult = referenceModel(testCase)
              refResults.enqueue(refResult)
            }
            case SERIAL => {
              input.valid #= true
              input.payload.x.raw #= Double2Fix(testCase.x)
              input.payload.y.raw #= Double2Fix(testCase.y)
              input.payload.z.raw #= Double2Fix(testCase.z)
              clockDomain.waitSampling()
              input.valid #= false
              val refResult = referenceModel(testCase)
              refResults.enqueue(refResult)
              clockDomain.waitSampling(cordic.getDelay)
            }
          }
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase) = {

    val x = testCase.x
    val y = testCase.y
    val z = testCase.z
    var xResult = 0.0
    var yResult = 0.0
    var zResult = 0.0
    //    val scaleFactor = 1.0 / CORDIC.getScaleComplement(20)(algebricMode)

    (rotationMode, algebricMode) match {
      case (ROTATION, CIRCULAR) => {
        xResult = x * cos(z) - y * sin(z)
        yResult = y * cos(z) + x * sin(z)
        zResult = 0.0
      }
      case (ROTATION, LINEAR) => {
        xResult = x
        yResult = y + x * z
        zResult = 0.0
      }
      case (ROTATION, HYPERBOLIC) => {
        xResult = x * cosh(z) - y * sinh(z)
        yResult = y * cosh(z) + x * sinh(z)
        zResult = 0.0
      }
      case (VECTORING, CIRCULAR) => {
        xResult = sqrt(x * x + y * y)
        zResult = z + atan(y / x)
        yResult = 0.0
      }
      case (VECTORING, LINEAR) => {
        xResult = x
        zResult = z + y / x
        yResult = 0.0
      }
      case (VECTORING, HYPERBOLIC) => {
        xResult = sqrt(x * x - y * y)
        zResult = z + atanh(y / x)
        yResult = 0.0
      }
    }

    Array(xResult, yResult, zResult)

  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (output.valid.toBoolean) {
          val dutResult = Array(output.payload.x, output.payload.y, output.payload.z).map(Fix2Double(_))
          dutResults.enqueue(dutResult)
        }
        clockDomain.waitSampling()
      }
    }
  }

  override def isValid(refResult: Array[Double], dutResult: Array[Double]): Boolean =
    sameFixedSeq(refResult, dutResult)

  override def messageWhenInvalid(refResult: Array[Double], dutResult: Array[Double]): String =
    s"\n result: ${dutResult.mkString(" ")} \n golden: ${refResult.mkString(" ")}"
}

object CORDICSim {
  private val r = Random

  private def randomCase(rotationMode: RotationMode, algebricMode: AlgebricMode) = {
    var x = 0.0
    var y = 0.0
    var z = 0.0

    (rotationMode, algebricMode) match {
      case (VECTORING, CIRCULAR) => {
        val rand = (r.nextDouble() - 0.5) * Pi
        x = cos(rand)
        y = sin(rand)
        z = 0.0
      }
      case (ROTATION, CIRCULAR) => {
        x = 1.0
        y = 0.0
        z = (r.nextDouble() - 0.5) * Pi
      }
      case (VECTORING, HYPERBOLIC) => {
        x = r.nextDouble()
        y = x * r.nextDouble() * 0.8
        z = 0.0
      }
      case (ROTATION, HYPERBOLIC) => {
        x = 1.0
        y = 0.0
        z = (r.nextDouble() - 0.5) * Pi * 0.5
      }
      case (VECTORING, LINEAR) => {
        x = r.nextDouble()
        y = (r.nextDouble() - 0.5) * x
        z = r.nextDouble() - 0.5
      }
      case (ROTATION, LINEAR) => {
        x = r.nextDouble() - 0.5
        y = r.nextDouble() - 0.5
        z = r.nextDouble() - 0.5
      }
    }

    cordicTestCase(x, y, z)
  }

  def randomSim(rotationMode: RotationMode, algebricMode: AlgebricMode, cordicArch: CordicArch): Unit = {

    val dut = SimConfig.withWave.compile(new CORDICSim(rotationMode, algebricMode, cordicArch))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 100) dut.insertTestCase(randomCase(rotationMode, algebricMode))
      dut.simDone()
    }
  }

  def main(args: Array[String]): Unit = {

    //    debug = true

    //    randomSim(VECTORING, CIRCULAR, PARALLEL)
    //    print(Console.GREEN)
    //    println(s"CORDIC VECTORING + CIRCULAR, PASS")
    //    print(Console.BLACK)
    //
    //    randomSim(ROTATION, CIRCULAR, PARALLEL)
    //    print(Console.GREEN)
    //    println(s"CORDIC ROTATION + CIRCULAR, PASS")
    //    print(Console.BLACK)
    //
    //    randomSim(VECTORING, HYPERBOLIC, PARALLEL)
    //    print(Console.GREEN)
    //    println(s"CORDIC VECTORING + HYPERBOLIC, PASS")
    //    print(Console.BLACK)
    //
    //    randomSim(ROTATION, HYPERBOLIC, PARALLEL)
    //    print(Console.GREEN)
    //    println(s"CORDIC ROTATION + HYPERBOLIC, PASS")
    //    print(Console.BLACK)
    //
    //    randomSim(ROTATION, LINEAR, PARALLEL)
    //    print(Console.GREEN)
    //    println(s"CORDIC ROTATION + LINEAR, PASS")
    //    print(Console.BLACK)
    //
    //    randomSim(VECTORING, LINEAR, PARALLEL)
    //    print(Console.GREEN)
    //    println(s"CORDIC VECTORING + LINEAR, PASS")
    //    print(Console.BLACK)

    randomSim(VECTORING, CIRCULAR, SERIAL)
    print(Console.GREEN)
    println(s"CORDIC VECTORING + CIRCULAR, PASS")
    print(Console.BLACK)
  }
}




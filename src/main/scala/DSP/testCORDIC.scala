package DSP

import DSP.AlgebricMode._
import DSP.RotationMode._
import breeze.numerics.constants.Pi
import breeze.numerics._
import spinal.core._
import spinal.core.sim._

import scala.util.Random

case class cordicTestCase(x: Double, y: Double, z: Double) {
  override def toString: String = s"x: $x, y: $y, z: ${z / Pi * 180.0} degree"
}

class testCORDIC(rotationMode: RotationMode, algebricMode: AlgebricMode)
  extends CORDIC(rotationMode = rotationMode, algebricMode = algebricMode)
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
          println("test: " + testCase.toString)
          referenceModel(testCase)
          input.valid #= true
          input.payload.x.raw #= Double2Fix(testCase.x)
          input.payload.y.raw #= Double2Fix(testCase.y)
          input.payload.z.raw #= Double2Fix(testCase.z)
          clockDomain.waitSampling()
          input.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {

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

    val golden = Array(xResult, yResult, zResult)
    refResults.enqueue(golden)
  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (output.valid.toBoolean) {
          val dutResult = Array(output.payload.x, output.payload.y, output.payload.z).map(Fix2Double)
          dutResults.enqueue(dutResult)
        }
        clockDomain.waitSampling()
      }
    }
  }

  override def scoreBoard(): Unit = {

    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          val same = refResult.zip(dutResult).map { case (ref, dut) => abs(ref - dut) }.forall(_ < 1E-1)
          assert(same, s"\n result: ${dutResult.mkString(" ")} \n golden: ${refResult.mkString(" ")}")
          //                    println(s"\n result: ${dutResult.mkString(" ")} \n golden: ${refResult.mkString(" ")}")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testCORDIC {
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
      case (VECTORING, HYPERBOLIC) => {
        x = r.nextDouble() * 16
        y = x * r.nextDouble() * 0.8
        z = 0.0
      }
      case (ROTATION, CIRCULAR) => {
        x = 1.0
        y = 0.0
        z = (r.nextDouble() - 0.5) * Pi
      }
    }

    cordicTestCase(x, y, z)
  }

  def randomSim(rotationMode: RotationMode, algebricMode: AlgebricMode): Unit = {

    val dut = SimConfig.withWave.compile(new testCORDIC(rotationMode, algebricMode))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 10) dut.insertTestCase(randomCase(rotationMode, algebricMode))
      dut.simDone()
    }
  }

  def main(args: Array[String]): Unit = {
    randomSim(VECTORING, CIRCULAR)
    print(Console.GREEN)
    println(s"CORDIC VECTORING + CIRCULAR, PASS")
    print(Console.BLACK)

    randomSim(VECTORING, HYPERBOLIC)
    print(Console.GREEN)
    println(s"CORDIC VECTORING + HYPERBOLIC, PASS")
    print(Console.BLACK)

    randomSim(ROTATION, CIRCULAR)
    print(Console.GREEN)
    println(s"CORDIC ROTATION + CIRCULAR, PASS")
    print(Console.BLACK)
  }
}




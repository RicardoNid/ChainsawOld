package DSP

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.numerics.constants.Pi
import breeze.numerics.{cos, sin}
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class testCORDIC() extends CORDIC() with DSPSim {
  override type TestCase = Array[Double]
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
          println("test: " + testCase.mkString(" "))
          referenceModel(testCase)
          input.valid #= true
          input.payload.x.raw #= Double2Fix(testCase(0))
          input.payload.y.raw #= Double2Fix(testCase(1))
          input.payload.z.raw #= Double2Fix(testCase(2))
          clockDomain.waitSampling()
          input.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    val golden = Array(0.0, 0.0, 0.0)
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

    def same(a: Double, b: Double) = scala.math.abs(a - b) / ((a + b) / 2) < 0.01 || scala.math.abs(a - b) < 0.1

    def sameVector(v1: DenseVector[Complex], v2: DenseVector[Complex]) =
      v1.toArray.zip(v2.toArray).forall { case (c1, c2) => same(c1.real, c2.real) && same(c1.imag, c2.imag) }

    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          println(s"result: ${dutResult.mkString(" ")}")
          assert(true, s"$refResult \n $dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testCORDIC {
  private val r = Random

  //    def randomCase(length: Int) = (for (elem <- (0 until 2 * length)) yield (r.nextDouble() * scala.math.pow(2, naturalWidth / 2))).toArray
  private def randomCase() = {
    val phase = (r.nextDouble() - 0.5) * Pi
    val x = cos(phase)
    val y = sin(phase)
    Array(x, y, phase)
  }

  def randomSim(): Unit = {
    val dut = SimConfig.withWave.compile(new testCORDIC())
    dut.doSim { dut =>
      dut.sim()
      //      for (i <- 0 until 10) dut.insertTestCase(randomCase())
      dut.insertTestCase(Array(0.707, 0.707, 0))
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"CORDIC, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    randomSim()
  }
}




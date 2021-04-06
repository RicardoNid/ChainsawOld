package DSP

import DSP.FIRArch._
import breeze.linalg.DenseVector
import breeze.signal._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ListBuffer
import scala.util.Random

class testFIR(coefficients: Array[Double], FIRArch: FIRArch) extends FIR(coefficients, FIRArch) with DSPSim {
  override type TestCase = Array[Double]
  override type ResultType = DenseVector[Double]

  override def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    io.input.valid #= false
    clockDomain.waitSampling(10)
  }

  override def simDone(): Unit = {
    clockDomain.waitSampling(10)
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(coefficients.length)
  }

  override def driver(): Unit = {
    val drv = fork {
      while (true) {
        if (testCases.nonEmpty) {
          val testCase = testCases.dequeue()
          referenceModel(testCase)
          for (data <- testCase) {
            io.input.valid #= true
            io.input.payload.raw #= Double2Fix(data)
            clockDomain.waitSampling()
          }
          io.input.valid #= false
          clockDomain.waitSampling(coefficients.length + 1)
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    val golden = filter(DenseVector(testCase), DenseVector(coefficients))
    refResults.enqueue(golden)
    //    println("ref enqueue")
  }

  override def monitor(): Unit = {
    val mon = fork {
      val outputBuffer = ListBuffer[Double]()
      while (true) {
        if (io.output.valid.toBoolean) {
          outputBuffer += Fix2Double(io.output.payload)
        }
        else {
          if (outputBuffer.nonEmpty) {
            dutResults.enqueue(DenseVector(outputBuffer.toArray))
            outputBuffer.clear()
            //            println("dut enqueue")
          }
        }
        clockDomain.waitSampling()
      }
    }
  }

  override def scoreBoard(): Unit = {

    def same(a: Double, b: Double) = scala.math.abs(a - b) / ((a + b) / 2) < 0.05 || scala.math.abs(a - b) < 0.5

    def sameVector(v1: DenseVector[Double], v2: DenseVector[Double]) =
      v1.toArray.zip(v2.toArray).forall { case (c1, c2) => same(c1, c2) }

    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          //          println(s"ref: $refResult \ndut: $dutResult")
          assert(sameVector(refResult, dutResult), s"\n $refResult \n $dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testFIR {
  private val r = Random

  private def randomCase(min: Int) = {
    val length = r.nextInt(100) + min
    (0 until length).map(i => randData()).toArray
  }

  def randomSim(): Unit = {
    val coeffLength = r.nextInt(20) + 5
    val randomCoeff = (0 until coeffLength).map(i => randData()).toArray
    val dut = SimConfig.withWave.compile(new testFIR(randomCoeff, MAC))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 10000) dut.insertTestCase(randomCase(coeffLength))
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"$coeffLength tap FIR, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    (0 until 20).foreach(i => randomSim())
  }
}


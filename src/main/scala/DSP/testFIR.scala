package DSP

import DSP.FIRArch._
import breeze.linalg.DenseVector
import breeze.signal._
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
          //          println(s"test: ${testCase.mkString(" ")}")
          val refResult = referenceModel(testCase)
          refResults.enqueue(refResult)
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

  override def referenceModel(testCase: TestCase) = {
    filter(DenseVector(testCase), DenseVector(coefficients))
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

  override def isValid(refResult: DenseVector[Double], dutResult: DenseVector[Double]): Boolean = sameFixedVector(refResult, dutResult)

  override def scoreBoard(): Unit = {

    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          println(s"ref: $refResult \ndut: $dutResult")
          assert(sameFixedVector(refResult, dutResult), s"\n $refResult \n $dutResult")
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
    (0 until length).map(i => randData(4)).toArray
  }

  def randomSim(arch: FIRArch): Unit = {
    val coeffLength = r.nextInt(4) + 3
    val randomCoeff = (0 until coeffLength).map(i => randData()).toArray
    println(randomCoeff.mkString(" "))
    val dut = SimConfig.withWave.compile(new testFIR(randomCoeff, arch))
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
    (0 until 20).foreach(i => randomSim(MAC))
    //    (0 until 20).foreach(i => randomSim(DA))
  }
}


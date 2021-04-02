package DSP

import spinal.core._
import spinal.core.sim._

import scala.util.Random

class testSCM(constant: Int, bitWidth: Int) extends SCM(constant, 16) with DSPSim {
  override type TestCase = Int
  override type ResultType = Int

  override def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    io.input.valid #= false
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
          referenceModel(testCase)
          io.input.valid #= true
          io.input.payload #= testCase
          clockDomain.waitSampling()
          io.input.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    val golden = constant * testCase
    refResults.enqueue(golden)
  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (io.output.valid.toBoolean) {

          val dutResult = io.output.payload.toInt
          println(s"dutResult: $dutResult")
          println(s"result: ${result.toInt} ")
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
          assert(refResult == dutResult, s"$refResult \n $dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testSCM {
  private val r = Random

  //    def randomCase(length: Int) = (for (elem <- (0 until 2 * length)) yield (r.nextDouble() * scala.math.pow(2, naturalWidth / 2))).toArray
  def randomSim(constant: Int): Unit = {
    val dut = SimConfig.withWave.compile(new testSCM(constant, 16))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 10000) dut.insertTestCase(r.nextInt(1000))
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"CSD with constant = $constant, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    (0 until 3).foreach(_ => randomSim(r.nextInt(4096)))
  }
}

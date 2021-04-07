package DSP

import spinal.core._
import spinal.core.sim._

import scala.util.Random

class testShiftAdderTree(shifts: Array[Int]) extends ShiftAdderTree(shifts) with DSPSim {
  override type TestCase = Array[Int]
  override type ResultType = Int

  override def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    inputs.valid #= false
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
          inputs.valid #= true
          //          println("test: " + testCase.mkString(" "))
          (0 until 5).foreach(i => inputs.payload(i) #= testCase(i))
          clockDomain.waitSampling()
          inputs.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    val golden = (0 until 5).map(i => testCase(i) << shifts(i)).sum
    refResults.enqueue(golden)
  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (output.valid.toBoolean) {
          val dutResult = output.payload.toInt
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
          //          println(s"\n ref: $refResult \n dut: $dutResult")
          assert(refResult == dutResult, s"\n$refResult \n$dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testShiftAdderTree {
  private val r = Random

  def randomCase = (0 until 5).map(i => r.nextInt(16)).toArray

  def randomSim(): Unit = {
    val dut = SimConfig.withWave.compile(new testShiftAdderTree(Array(3, 7, 8, 5, 1)))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 100) dut.insertTestCase(randomCase)
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"shiftAdderTree, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    randomSim()
  }
}




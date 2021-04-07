package DSP

import spinal.core._
import spinal.core.sim._

import scala.util.Random

class testShiftAdderTree(shifts: IndexedSeq[Int]) extends ShiftAdderTree(shifts) with DSPSim {
  override type TestCase = IndexedSeq[Int]
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
          (0 until shifts.length).foreach(i => inputs.payload(i) #= testCase(i))
          clockDomain.waitSampling()
          inputs.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    val golden = (0 until shifts.length).map(i => testCase(i) << shifts(i)).sum
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
          //                    println(s"\n ref: $refResult \n dut: $dutResult")
          assert(refResult == dutResult, s"\n$refResult \n$dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testShiftAdderTree {
  private val r = Random

  def randomShifts = {
    val length = r.nextInt(32)
    val res = (0 until length).map(i => r.nextInt(10))
    println(res.mkString(" "))
    res
  }

  def randomCase(length: Int) = (0 until length).map(i => r.nextInt(64)).toArray

  def randomSim(shifts: IndexedSeq[Int]): Unit = {
    val dut = SimConfig.withWave.compile(new testShiftAdderTree(shifts))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 100) dut.insertTestCase(randomCase(shifts.length))
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"shiftAdderTree with ${shifts.mkString(" ")}, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    (0 until 100).foreach(i => randomSim(randomShifts))
  }
}




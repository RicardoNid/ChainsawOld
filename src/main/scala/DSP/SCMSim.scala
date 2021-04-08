package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random

class SCMDUT(constant: Int, bitWidth: Int = naturalWidth, scmArch: SCMArch) extends Component with DSPGen {

  val input = slave Flow SInt(bitWidth bits)
  val output = master Flow SInt

  val body = SCM(input.payload, constant, scmArch)

  output.payload := body.implicitValue
  output.valid := Delay(input.valid, body.delay, init = False)
}

object SCMDUT {
  def main(args: Array[String]): Unit = {
  }
}

class SCMSim(constant: Int, bitWidth: Int, scmArch: SCMArch) extends SCMDUT(constant, bitWidth, scmArch) with DSPSim {
  override type TestCase = Int
  override type ResultType = Int

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
          input.valid #= true
          input.payload #= testCase
          clockDomain.waitSampling()
          input.valid #= false
          referenceModel(testCase)
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    val golden = constant * testCase
    printlnWhenDebug(s"time $simTime: refResult = $golden")
    refResults.enqueue(golden)
  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (output.valid.toBoolean) {

          val dutResult = output.payload.toInt
          printlnWhenDebug(s"time $simTime: dutResult = $dutResult")
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

object SCMSim {
  private val r = Random

  //    def randomCase(length: Int) = (for (elem <- (0 until 2 * length)) yield (r.nextDouble() * scala.math.pow(2, naturalWidth / 2))).toArray
  def randomSim(constant: Int, scmArch: SCMArch): Unit = {
    val dut = SimConfig.withWave.compile(new SCMSim(constant, 16, scmArch))
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
    //    debug = true
    randomSim(93, SCMArch.CSD)
  }
}


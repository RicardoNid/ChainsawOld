package Chainsaw.MCM

import Chainsaw.MCM.SCMArch._
import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

class SCMSim(constant: Int, scmArch: SCMArch) extends Component with DSPSim[SReal, SReal, Double, Double] {
  override val input: Flow[SReal] = slave Flow SReal(IntRange(0, 4096))

  val scm = new SCM(input.payload, constant, scmArch)
  val ret = scm.implicitValue

  override val output: Flow[SReal] = master Flow ret
  output.payload := ret
  override val timing: TimingInfo = scm.getTimingInfo
  output.valid := Delay(input.valid, timing.latency, init = False)

  override def poke(testCase: Double, input: SReal): Unit = {
    input #= testCase
    clockDomain.waitSampling()
  }
  override def peek(output: SReal): Double = {
    val ret = output.toDouble
    clockDomain.waitSampling()
    ret
  }
  override def referenceModel(testCase: Double): Double = testCase * constant
  override def isValid(refResult: Double, dutResult: Double): Boolean = refResult == dutResult
  override def messageWhenInvalid(testCase: Double, refResult: Double, dutResult: Double): String = s"golden: $refResult, yours: $dutResult"
  override def messageWhenValid(testCase: Double, refResult: Double, dutResult: Double): String = s"golden: $refResult, yours: $dutResult"
}

object SCMSim {

  def randomSim(constant: Int, scmArch: SCMArch): Unit = {
    val dut = SimConfig.withWave.compile(new SCMSim(constant, scmArch))
    dut.doSim { dut =>
      dut.sim()
      for (_ <- 0 until 100) dut.insertTestCase(DSPRand.nextInt(1023))
      println(dut.simDone())
    }
    print(Console.GREEN)
    val mode = scmArch match {
      case CSD => "CSD"
      case SCMArch.MAG => "MAG"
      case MULT => "MULT"
    }
    println(s"$mode with constant = $constant, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    debug = true
    import AOperations.getPOF
    (0 until 5).foreach(_ => randomSim(getPOF(DSPRand.nextInt(1023)), SCMArch.CSD))
    (0 until 5).foreach(_ => randomSim(getPOF(DSPRand.nextInt(1023)), SCMArch.MAG))
  }
}


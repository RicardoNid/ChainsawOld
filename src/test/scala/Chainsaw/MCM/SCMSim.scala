package Chainsaw.MCM

import Chainsaw.tobeTransplanted.SCMArch._
import Chainsaw._
import Chainsaw.tobeTransplanted.{SCM, SCMArch}
import spinal.core._
import spinal.core.sim._

import scala.math.abs

class SCMDUT(constant: Int, scmArch: SCMArch) extends DSPDUTTiming[Real, Real] {
  override val input = in(Real(-1.5, 1, -15 exp).withRoundingError)
  //  override val input: Flow[Real] = slave Flow SIntReal(-3, 6)
  val scm = new SCM(input, constant, scmArch)
  val ret = scm.implicitValue
  override val output = out(scm.implicitValue)
  override val timing: TimingInfo = scm.getTimingInfo
}

class SCMSim(constant: Int, scmArch: SCMArch) extends
  SCMDUT(constant, scmArch)
  with DSPSimTiming[Real, Real, Double, Double] {

  override def poke(testCase: Double, input: Real): Unit = {
    input #= testCase
    clockDomain.waitSampling()
  }

  override def peek(output: Real): Double = {
    val ret = output.toDouble
    clockDomain.waitSampling()
    ret
  }

  override def referenceModel(testCase: Double): Double = testCase * constant

  override def isValid(refResult: Double, dutResult: Double): Boolean = abs(refResult - dutResult) <= ret.error

  override def messageWhenInvalid(testCase: Double, refResult: Double, dutResult: Double): String = s"golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: Double, refResult: Double, dutResult: Double): String = s"golden: $refResult, yours: $dutResult"
}

object SCMSim {

  def randomSim(constant: Int, scmArch: SCMArch): Unit = {
    val dut = SimConfig.withWave.compile(new SCMSim(constant, scmArch))
    dut.doSim { dut =>
      dut.sim()
      for (_ <- 0 until 100) dut.insertTestCase(dut.input.randomValue())
      val report = dut.simDone()
      val mode = scmArch match {
        case CSD => "CSD"
        case SCMArch.MAG => "MAG"
        case MULT => "MULT"
      }
      if (report.totalCase == report.trueCase) printlnGreen(s"$mode with constant = $constant, PASS")
      else printlnRed(s"$mode with constant = $constant, FAIL")
    }
  }

  def main(args: Array[String]): Unit = {
    ChainsawDebug = true
    import Chainsaw.tobeTransplanted.AOperations.getPOF
    (0 until 5).foreach(_ => randomSim(getPOF(DSPRand.nextInt(1023)), SCMArch.CSD))
    (0 until 5).foreach(_ => randomSim(getPOF(DSPRand.nextInt(1023)), SCMArch.MAG))
    (0 until 5).foreach(_ => randomSim(getPOF(DSPRand.nextInt(1023)), SCMArch.MULT))
  }
}
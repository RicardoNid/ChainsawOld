package Chainsaw.FloPoCo.Transplanted

import Chainsaw.{Real, _}
import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._

import scala.math.abs

class SCMDUT(constant: Int) extends Component with DSPDUTTiming[Real, Real] {
  override val input: Real = in(Real(-1.5, 2.5, 0.1))
  val scm = new SCM(input, constant)
  override val output: Real = out(scm.implicitValue)
  override val timing: TimingInfo = scm.getTimingInfo
}

class SCMSim(constant: Int) extends SCMDUT(constant) with DSPSimTiming[Real, Real, Double, Double] {
  override def poke(testCase: Double, input: Real): Unit = input #= testCase

  override def peek(output: Real): Double = output.toDouble

  override def referenceModel(testCase: Double): Double = testCase * constant

  override def isValid(refResult: Double, dutResult: Double): Boolean = abs(refResult - dutResult) <= output.error

  override def messageWhenInvalid(testCase: Double, refResult: Double, dutResult: Double): String =
    s"testCase: $testCase, golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: Double, refResult: Double, dutResult: Double): String =
    s"testCase: $testCase, golden: $refResult, yours: $dutResult"
}

class testSCM extends FunSuite{
  def randomSim(constant: Int, traversal: Boolean = false): Unit = {
    val dut = SimConfig.withWave.compile(new SCMSim(constant))
    dut.doSim { dut =>
      dut.sim()
      if (traversal) {
        println(s"${dut.input.allValues.length} cases to be tested")
        dut.input.allValues.foreach(dut.insertTestCase)
      }
      else for (_ <- 0 until 100) dut.insertTestCase(dut.input.randomValue())
      val report = dut.simDone()
      if (report.totalCase == report.trueCase) printlnGreen(s"With constant = $constant, PASS")
      else printlnRed(s"With constant = $constant, FAIL")
    }
  }

  test("testTranplantedSCM") {
    ChainsawDebug = true
    //    randomSim(957) // by this, the pattern match is fixed
    // in this case, we noticed that sometimes, +/- leads to a narrower interval and should be processed
    randomSim(171, traversal = true)
    (0 until 100).foreach(_ => randomSim(Chainsaw.MCM.AOperations.getPOF(DSPRand.nextInt(1023))))
  }
}

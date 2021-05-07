package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._

class SCMSim(constant: Int, scmArch: SCMArch) extends Component with DSPSim[SInt, SInt, Int, Int] {
  override val input: Flow[SInt] = slave Flow SInt(12 bits)
  override val output: Flow[SInt] = master Flow SInt(12 + log2Up(constant) bits)

  val scm = new SCM(input.payload, constant, scmArch)
  output.payload := scm.implicitValue.resized
  override val timing: TimingInfo = scm.getTimingInfo
  output.valid := Delay(input.valid, timing.latency, init = False)

  override def poke(testCase: Int, input: SInt): Unit = {
    input #= testCase
    clockDomain.waitSampling()
  }
  override def peek(output: SInt): Int = {
    val ret = output.toInt
    clockDomain.waitSampling()
    ret
  }
  override def referenceModel(testCase: Int): Int = testCase * constant
  override def isValid(refResult: Int, dutResult: Int): Boolean = refResult == dutResult
  override def messageWhenInvalid(testCase: Int, refResult: Int, dutResult: Int): String = s"golden: $refResult, yours: $dutResult"
  override def messageWhenValid(testCase: Int, refResult: Int, dutResult: Int): String = s"golden: $refResult, yours: $dutResult"
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
    println(s"CSD with constant = $constant, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    debug = true
    (0 until 10).foreach(_ => randomSim(DSPRand.nextInt(1023), SCMArch.CSD))
  }
}


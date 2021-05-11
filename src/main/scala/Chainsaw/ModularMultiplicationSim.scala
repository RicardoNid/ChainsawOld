package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, Flow, master, slave}

import scala.util.Random

/** Kernel for modular multiplication by Montgomery
 *
 * @param N modulo
 */
//class ModularMultiplicationSim(N: Int) extends Component {
//  val input = slave Flow Vec(UInt(log2Up(N - 1) bits), 2)
//  val output = master Flow UInt(log2Up(N - 1) bits)
//
//  val mm = new ModularMultiplication(input.payload(0), input.payload(1), N)
//  output.payload := mm.implicitValue
//  val timing: TimingInfo = mm.getTimingInfo
//  output.valid := Delay(input.valid, timing.latency, False)
//}
//
//object ModularMultiplicationSim {
//  def main(args: Array[String]): Unit = {
//    SpinalConfig().generateSystemVerilog(new ModularMultiplicationSim(13))
//  }
//}

class ModularMultiplicationSim(N: Int) extends Component with DSPSim[Vec[UInt], UInt, (Int, Int), Int] {
  val innerWidth = log2Up(N - 1)

  override val input: Flow[Vec[UInt]] = slave Flow Vec(UInt(innerWidth bits), 2)
  override val output: Flow[UInt] = master Flow UInt(innerWidth bits)

  val mm = new ModularMultiplication(input.payload(0), input.payload(1), N)
  override val timing: TimingInfo = mm.getTimingInfo
  output.payload := mm.implicitValue
  output.valid := Delay(input.valid, timing.latency, init = False)

  override def poke(testCase: (Int, Int), input: Vec[UInt]): Unit = {
    input(0) #= testCase._1
    input(1) #= testCase._2
    clockDomain.waitSampling()
  }

  override def peek(output: UInt): Int = {
    val ret = output.toInt
    clockDomain.waitSampling()
    ret
  }

  override def referenceModel(testCase: (Int, Int)): Int = (testCase._1 * testCase._2) % N

  override def isValid(refResult: Int, dutResult: Int): Boolean = refResult == dutResult

  override def messageWhenInvalid(testCase: (Int, Int), refResult: Int, dutResult: Int): String =
    s"\ntestCase: ${testCase}, golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: (Int, Int), refResult: Int, dutResult: Int): String =
    s"\ntestCase: ${testCase}, golden: $refResult, yours: $dutResult"
}


object ModularMultiplicationSim {
  def main(args: Array[String]): Unit = {
    debug = true
    val dut = SimConfig.withWave.compile(new ModularMultiplicationSim(133))
    dut.doSim { dut =>
      dut.sim()
      val r = new Random
      (0 until 100).foreach(_ => dut.insertTestCase(r.nextInt(133), r.nextInt(133)))
      val report = dut.simDone()
      println(report.validLog.mkString(""))
    }

    //    val report = VivadoFlow(design = new ModularMultiplicationSim(133), workspacePath = "output/MM", topModuleName = "MM").doit()
    //    report.printArea
    //    report.printFMax
  }
}




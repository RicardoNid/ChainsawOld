package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._

/** Test BinaryTree by implmenting integer addtions
 *
 */
class BinaryTreeSim extends Component with DSPSimLatest[Vec[SReal], SReal, Array[Int], Int] { // TODO: test it with real numbers
  override val input: Flow[Vec[SReal]] = slave Flow Vec(SReal(IntRange(0, 127)), 17)
  override val output: Flow[SReal] = master Flow SReal(IntRange(0, 10000))

  val add = (x: SReal, y: SReal) => x + y

  val binaryTree = new BinaryTree(input.payload, add)
  output.payload := binaryTree.implicitValue
  output.valid := Delay(input.valid, binaryTree.getTimingInfo.latency, init = False)

  override val timing: TimingInfo = binaryTree.getTimingInfo

  override def poke(testCase: Array[Int], input: Vec[SReal]): Unit = {
    input.zip(testCase).foreach { case (real, i) => real.raw #= i }
    clockDomain.waitSampling()
  }

  override def peek(output: SReal): Int = {
    val ret = output.raw.toInt
    clockDomain.waitSampling()
    ret
  }

  override def referenceModel(testCase: Array[Int]): Int = testCase.sum

  override def isValid(refResult: Int, dutResult: Int): Boolean = refResult == dutResult

  override def messageWhenInvalid(testCase: Array[Int], refResult: Int, dutResult: Int): String =
    s"testCase: ${testCase}, golden: ${refResult}, yours: ${dutResult}"

  override def messageWhenValid(testCase: Array[Int], refResult: Int, dutResult: Int): String =
    s"testCase: ${testCase}, golden: ${refResult}, yours: ${dutResult}"
}

object TreesSim {
  def main(args: Array[String]): Unit = {

    debug = true

    println("start testing BinaryTree")
    SimConfig.compile(new BinaryTreeSim).doSim { dut =>
      dut.sim()
      dut.insertTestCase((0 until 17).toArray)
      dut.insertTestCase((0 until 17).toArray)
      dut.insertTestCase((0 until 17).toArray)
      val report = dut.simDone()
      println(report.log.mkString("\n"))
      println(report.validLog.mkString("\n"))
    }
  }
}
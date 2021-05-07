package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._

/** Test BinaryTree by implmenting integer addtions
 *
 */
class BinaryTreeSim(length: Int, opertor: (SReal, SReal) => SReal, pipelineInterval: Int, refOperator: (Int, Int) => Int) extends Component with DSPSimLatest[Vec[SReal], SReal, Array[Int], Int] { // TODO: test it with real numbers
  override val input: Flow[Vec[SReal]] = slave Flow Vec(SReal(IntRange(0, 127)), length)
  override val output: Flow[SReal] = master Flow SReal(IntRange(0, 10000))

  println(output.payload.range)

  val binaryTree = new BinaryTree(input.payload, opertor, pipelineInterval)
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

  override def referenceModel(testCase: Array[Int]): Int = testCase.reduce(refOperator(_, _))

  override def isValid(refResult: Int, dutResult: Int): Boolean = refResult == dutResult

  override def messageWhenInvalid(testCase: Array[Int], refResult: Int, dutResult: Int): String =
    s"testCase: ${testCase.mkString(" ")}, golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: Array[Int], refResult: Int, dutResult: Int): String =
    s"testCase: ${testCase.mkString(" ")}, golden: $refResult, yours: $dutResult"
}

object TreesSim {
  def main(args: Array[String]): Unit = {

    debug = true

    println("start testing BinaryTree")
    val add = (x: SReal, y: SReal) => x + y // Fixme: numeric operations in Real is currently a mess!
    val mul = (x: SReal, y: SReal) => x * y
    println("test with addtion")
    SimConfig.withWave.compile(new BinaryTreeSim(17, add, 0, _ + _)).doSim { dut =>

      def random = (0 until 17).map(_ => DSPRand.nextInt(127)).toArray

      dut.sim()
      (0 until 100).foreach(_ => dut.insertTestCase(random))
      println(dut.simDone())
    }
    println("test with multiplication")
    SimConfig.compile(new BinaryTreeSim(9, mul, 2, _ * _)).doSim { dut =>
      dut.sim()
      dut.insertTestCase((0 until 9).toArray)
      dut.insertTestCase((1 until 10).toArray)
      dut.insertTestCase((2 until 11).toArray)
      println(dut.simDone())
    }
  }
}
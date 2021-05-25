package Chainsaw.Architectures

import Chainsaw.{Real, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class BinaryTreeDUT(length: Int, opertor: (Real, Real) => Real, pipelineInterval: Int, refOperator: (Double, Double) => Double)
  extends DSPDUTTiming[Vec[Real], Real] {
  override val input = in Vec(Real(-1, 1, 0.001), length)
  val binaryTree = BinaryTree(input, opertor, pipelineInterval)
  override val output = out(binaryTree.implicitValue)
  override val timing: TimingInfo = binaryTree.getTimingInfo
}

/** Test BinaryTree by implementing integer addtions and multiplications
 *
 */
class BinaryTreeSim(length: Int, opertor: (Real, Real) => Real, pipelineInterval: Int, refOperator: (Double, Double) => Double)
  extends BinaryTreeDUT(length, opertor, pipelineInterval, refOperator)
    with DSPSimTiming[Vec[Real], Real, Array[Double], Double] { // TODO: test it with real numbers

  override def poke(testCase: Array[Double], input: Vec[Real]): Unit = {
    input.zip(testCase).foreach { case (real, d) => real #= d }
    clockDomain.waitSampling()
  }

  override def peek(output: Real): Double = {
    val ret = output.toDouble
    clockDomain.waitSampling()
    ret
  }

  override def referenceModel(testCase: Array[Double]): Double = testCase.reduce(refOperator(_, _))

  override def isValid(refResult: Double, dutResult: Double): Boolean = refResult == dutResult

  override def messageWhenInvalid(testCase: Array[Double], refResult: Double, dutResult: Double): String =
    s"testCase: ${testCase.mkString(" ")}, golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: Array[Double], refResult: Double, dutResult: Double): String =
    s"testCase: ${testCase.mkString(" ")}, golden: $refResult, yours: $dutResult"
}

class testBinaryTree extends AnyFunSuite {
  test("testBinaryTree") { // TODO: test on Double

    println("start testing BinaryTree")
    val add = (x: Real, y: Real) => x + y
    val mul = (x: Real, y: Real) => x * y

    // testCase at the precision of 1/16, which is the nearest power of 2 to 0.1
    def random(length: Int) = (0 until length).map(_ => (DSPRand.nextInt(20) - 10) / 16.0).toArray

    println("test with addtion")
    SimConfig.withWave.compile(new BinaryTreeSim(17, add, 0, _ + _)).doSim { dut =>

      dut.sim()
      (0 until 100).foreach(_ => dut.insertTestCase(random(17)))
      print(Console.GREEN)
      println(dut.simDone())
      print(Console.BLACK)
    }

    println("test with multiplication")
    SimConfig.compile(new BinaryTreeSim(3, mul, 2, _ * _)).doSim { dut =>
      dut.sim()
      (0 until 100).foreach(_ => dut.insertTestCase(random(3)))
      print(Console.GREEN)
      val report = dut.simDone()
      println(report)
      println(report.validLog)
      print(Console.BLACK)
    }
  }
}
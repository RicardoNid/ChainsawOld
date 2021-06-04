package FTN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

import scala.collection.mutable.ArrayBuffer

class ParallelFrontEndSim extends ParallelFrontEnd with DSPSimTiming[Bits, Bits, Array[Array[Int]], Array[Array[Int]]] {

  override def simInit(): Unit = {
    clockDomain.forkStimulus(period)
    sleep(32)
    start #= true
  }

  override def poke(testCase: Array[Array[Int]], input: Bits): Unit = {} // do nothing
  override def peek(output: Bits): Array[Array[Int]] = {
    val container = ArrayBuffer[Array[Int]]()
    (0 until timing.outputInterval - 1).foreach { _ =>
      container +=
        output.toBigInt.toString(2)
          .reverse.padTo(896, '0').reverse
          .map(char => if (char == '1') 1 else 0)
          .toArray
      clockDomain.waitSampling()
    }
    container += output.toBigInt.toString(2).reverse.padTo(896, '0').reverse.map(char => if (char == '1') 1 else 0).toArray
    container.grouped(16).map { slices => // 16 * 896
      val transpose = (0 until 896).map(i => slices.map(slice => slice(i))) // 896 * 16
      transpose.flatten.toArray
    }.toArray
  }
  override def referenceModel(testCase: Array[Array[Int]]): Array[Array[Int]] = testCase
  override def isValid(refResult: Array[Array[Int]], dutResult: Array[Array[Int]]): Boolean = false
  override def messageWhenInvalid(testCase: Array[Array[Int]], refResult: Array[Array[Int]], dutResult: Array[Array[Int]]): String =
    s"testCase first 16 bits: ${testCase(0).take(16).mkString(" ")}, dutResult first 32 bits: ${dutResult(0).take(32).mkString(" ")}"
  override def messageWhenValid(testCase: Array[Array[Int]], refResult: Array[Array[Int]], dutResult: Array[Array[Int]]): String =
    refResult(0).mkString("")
}

class ParallelFrontEndTest extends AnyFunSuite {
  test("testParallelFrontEnd") {
    ChainsawDebug = true
    SimConfig.withWave.compile(new ParallelFrontEndSim).doSim { dut =>
      dut.sim()
      dut.insertTestCase(
        (0 until 10).map(_ =>
          (0 until 7168).map(_ => DSPRand.nextInt(2)).toArray).toArray)
      val report = dut.simDone()
      println(report)
    }
  }
}

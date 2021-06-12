package Chainsaw.Crypto.RSA

import Chainsaw.printPadded
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimConfig, _}
import spinal.core.{U, log2Up, _}

import scala.collection.mutable.ArrayBuffer

class MultiplierCombinatorTest extends AnyFunSuite {

  test("testMultiplierCombinator") {
    val groupSize = 6
    val baseWidth = 8
    val expansionFactor = 4
    val round = 3
    val period = groupSize * expansionFactor * expansionFactor
    SimConfig.withWave
      .compile {
        new MultiplierCombinator(baseWidth, expansionFactor, Mult.apply, Add.apply, groupSize) {
          add.input.setName("add_input")
          add.output.setName("add_output")
          mult.input.setName("mult_input")
          mult.output.setName("mult_output")
          val outputRAM_0 = outputRAM(U(0, log2Up(groupSize) bits))
          outputRAM_0.simPublic()
          valid.simPublic()
        }
      }
      .doSim { dut =>
        import dut._
        val inputs = ArrayBuffer[(BigInt, BigInt)]()
        val dutResults = ArrayBuffer[BigInt]()

        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()
        (0 until round * period + groupSize).foreach { i =>
          if ((i % period) < groupSize) {
            val input0 = dut.input(0).randomizedBigInt()
            val input1 = dut.input(1).randomizedBigInt()
            inputs += Tuple2(input0, input1)
            dut.input(0) #= input0
            dut.input(1) #= input1
          } else {
            dut.input(0) #= 0
            dut.input(1) #= 0
          }
          if (valid.toBoolean) dutResults += output.toBigInt
          clockDomain.waitSampling()
        }
        inputs.zipWithIndex.foreach { case (tuple, i) =>
          printPadded(s"result $i", tuple._1 * tuple._2, expandedWidth * 2)
        }
        assert(dutResults.size != 0)
        inputs.zip(dutResults).foreach { case (tuple, int) => assertResult(tuple._1 * tuple._2)(int) }
      }
  }
}

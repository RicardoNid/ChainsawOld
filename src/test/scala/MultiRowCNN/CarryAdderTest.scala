package MultiRowCNN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

class CarryAdderTest extends AnyFunSuite {

  test("test CarryAdder"){
    SimConfig.withWave.compile(new CarryAdder(4)).doSim{ dut =>
      import dut.{a, b, result}
      val testCaseA = 13
      val testCaseB = 13

      a #= 13.5
      b #= 15.2

      sleep(2)
      println(result.toDouble)

      val dutResult = result.toDouble
      val golden = 13.5 + 15.2

      assert(doubleEquals(golden,dutResult,0.05, 0.1))
    }
  }
}

package FTN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class TrainCtrlTest extends AnyFunSuite {

  test("test TrainCtrl for FTN"){
    SimConfig.withWave.compile(new TrainCtrl(137, 16, 10)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      input.valid #= false
      output.ready #= true
      clockDomain.waitSampling()

      (0 until 5).foreach { i =>
        (0 until 16).foreach { j =>
          input.valid #= true
          input.payload #= i * 100 + j
          clockDomain.waitSampling()
        }
      }
      input.valid #= false
      input.payload #= 0
      clockDomain.waitSampling(2000)
    }
  }
}

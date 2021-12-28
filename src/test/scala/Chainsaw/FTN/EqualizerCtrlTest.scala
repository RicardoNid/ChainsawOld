package Chainsaw.FTN

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

class EqualizerCtrlTest extends AnyFlatSpec {

  behavior of "EqualizerCtrl"

  it should "work" in {
    GenRTL(EqualizerCtrl())
    SimConfig.withWave.compile(EqualizerCtrl()).doSim { dut =>

      dut.dataIn.valid #= false
      dut.dataOut.ready #= true

      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()

      def runMode0(): Unit = {
        (0 until 80).foreach { i =>
          if (i < 18) {
            dut.dataIn.valid #= true
            dut.dataIn.payload #= i
          }
          else dut.dataIn.valid #= false
          dut.clockDomain.waitSampling()
        }
      }

      val dutResults = ArrayBuffer[BigInt]()
      dut.dataOut.setMonitor(dutResults)

      (0 until 10).foreach(_ => runMode0())
      dut.clockDomain.waitSampling(10)

      val goldenResults = (0 until 10).map(_ => 2 until 18).flatten
      assert(dutResults.zip(goldenResults).forall{ case (a, b) => a.intValue() == b})

    }
  }

}

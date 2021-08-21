package Chainsaw.Communication

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real
import org.scalatest.funsuite.AnyFunSuite

class QAMModTest extends AnyFunSuite {

  val pF = 256
  val bitPerSymbol = 4

  val bitAlloc = Seq.fill(pF)(bitPerSymbol)
  val powerAlloc = Seq.fill(pF)(1.0)
  val symbolType = HardType(ComplexNumber(1, -14))

  SimConfig.withWave.compile(new QAMMod(bitAlloc, powerAlloc, symbolType)).doSim{dut =>
    import dut.{clockDomain, dataIn, dataOut}
    clockDomain.forkStimulus(2)
    dataIn.valid #= false
    clockDomain.waitSampling()

    val testCase = DSPRand.nextBigInt(pF * bitPerSymbol)
    dataIn.payload #= testCase
    dataIn.valid #= true
    clockDomain.waitSampling(2)

    assertResult(expected = eng.feval())


  }
}

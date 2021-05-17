package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw.Real
import Chainsaw._

class HandShakeExample extends Component with DSPDUTHandShake[UInt, UInt] {

  val input = slave Stream UInt(2 bits)
  val output = master Stream UInt(3 bits)

  val isActiveIDLE = Bool()
  val isNextIDLE = Bool()
  val isCompletedIDLE = Bool()
  val isInactiveIDLE = Bool()
  val isOnEntryIDLE = Bool()
  val isOnExitIDLE = Bool()

  val flags = Seq(isActiveIDLE, isNextIDLE, isCompletedIDLE, isInactiveIDLE, isOnEntryIDLE, isOnExitIDLE)
  out(flags: _*)
  flags.foreach(_ := False)

  val acc = RegInit(U(0, 3 bits))
  val counter = Counter(3)

  output.valid := False
  input.ready := True

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val RUN = new State()
    val REST = new StateDelay(1).whenCompleted {
      when(input.fire)(goto(RUN))
        .otherwise(goto(IDLE))
    }

    IDLE
      .whenIsActive {
        when(input.fire)(goto(RUN))
      }
    RUN
      .whenIsActive(isActiveIDLE := True)
      .whenIsNext(isNextIDLE := True)
      .whenIsInactive(isInactiveIDLE := True)
      .onExit(isOnExitIDLE := True)
      .onEntry(isOnEntryIDLE := True)

      .whenIsActive {
        when(counter.willOverflow)(goto(REST))
        counter.increment()
        when(counter.willOverflow)(acc := input.payload.resized)
          .otherwise(acc := acc + input.payload)
        when(counter.willOverflow)(output.valid := True)
        when(counter.willOverflow)(input.ready := False)
      }
      .onEntry {
        counter.clear()
        acc := input.payload.resized
      }
  }
  output.payload := acc.resized
}

class HandShakeExampleSim extends HandShakeExample with DSPSimHandShake[UInt, UInt, Seq[Int], Int] {
  override def poke(testCase: Seq[Int], input: UInt): Unit = {
    input #= testCase(0)
    clockDomain.waitSampling()
    input #= testCase(1)
    clockDomain.waitSampling()
    input #= testCase(2)
  }

  override def peek(output: UInt) = output.toInt

  override def referenceModel(testCase: Seq[Int]) = testCase.sum

  override def isValid(refResult: Int, dutResult: Int) = true

  override def messageWhenInvalid(testCase: Seq[Int], refResult: Int, dutResult: Int) =
    s"testCase: ${testCase.mkString(" ")}, golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: Seq[Int], refResult: Int, dutResult: Int) =
    s"testCase: ${testCase.mkString(" ")}, golden: $refResult, yours: $dutResult"

  override type RefOwnerType = this.type
}

object HandShakeExampleSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new HandShakeExampleSim).doSim { dut =>
      //      dut.clockDomain.forkStimulus(2)
      //      dut.input.valid #= true
      //      dut.output.ready #= true
      //      dut.input.payload #= 1
      //      sleep(100)
      //      simSuccess()
      dut.sim()
      dut.insertTestCase(Seq(1, 2, 3))
      dut.insertTestCase(Seq(1, 1, 1))
      dut.insertTestCase(Seq(2, 2, 3))
      dut.simDone()
    }
  }
}

package tutorial.HDLB.fancytimer

import tutorial.HDLB.StateMachineNoBoot
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import sysu.util._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Exams/review2015_fsm
class CombinedFSM extends Component {

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val data = in Bool
    val done_counting = in Bool
    val ack = in Bool

    val shift_ena = out Bool
    val counting = out Bool
    val done = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val fsm = new StateMachineNoBoot{
      val WAITING = StateEntryPoint()
      val SHIFTING = new StateDelay(4).whenCompleted(goto(COUNTING))
      val COUNTING = State()
      val DONE = State()

      val seqRec = SeqRec("1101", io.data)

      // state trasition logic
      WAITING
        .onEntry(seqRec.clear())
        .whenIsActive(when(seqRec.willRecognize)(goto(SHIFTING)))
      COUNTING
        .whenIsActive(when(io.done_counting)(goto(DONE)))
      DONE
        .whenIsActive(when(io.ack)(goto(WAITING)))

      io.shift_ena := isActive(SHIFTING)
      io.counting := isActive(COUNTING)
      io.done := isActive(DONE)
    }
  }

  noIoPrefix()
}

object CombinedFSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "CombinedFSM.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new CombinedFSM().setDefinitionName("top_module"))
  }
}

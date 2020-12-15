package HDLB.fancytimer

import HDLB.StateMachineNoBoot
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Exams/review2015_fancytimer
class FancyTimer extends Component {

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val data = in Bool
    val ack = in Bool

    val count = out UInt (4 bits)
    val counting = out Bool
    val done = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val fsm = new StateMachineNoBoot {
      val WAITING = StateEntryPoint()
      val SHIFTING = new StateDelay(4).whenCompleted(goto(COUNTING))
      val COUNTING = State()
      val DONE = State()

      val seqRec = mylib.SeqRec("1101", io.data)
      val countReg = RegInit(U(0, 4 bits))
      val count1000 = Counter(1000, isActive(COUNTING))

      // state trasition logic
      WAITING
        .onEntry(seqRec.clear())
        .whenIsActive(when(seqRec.willRecognize)(goto(SHIFTING)))
      SHIFTING
        .whenIsActive{
          countReg :=  (countReg << 1).resize(4)
          countReg(0) := io.data
        }
      COUNTING
        .whenIsActive{
          when(count1000.willOverflow && countReg === U(0))(goto(DONE))
            .elsewhen(count1000.willOverflow)(countReg := countReg - 1)
            .otherwise()
        }
      DONE
        .whenIsActive(when(io.ack)(goto(WAITING)))

      // output logic
      io.count := countReg
      io.counting := isActive(COUNTING)
      io.done := isActive(DONE)
    }
  }

  noIoPrefix()
}

object FancyTimer {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "FancyTimer.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new FancyTimer().setDefinitionName("top_module"))
  }
}

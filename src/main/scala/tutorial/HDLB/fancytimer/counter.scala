package tutorial.HDLB.fancytimer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Exams/review2015_shiftcount
class counter extends Component {

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val shift_ena = in Bool
    val count_ena = in Bool
    val data = in Bool

    val q = out UInt (4 bits)
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {

    val qReg = Reg(UInt(4 bits))
    when(io.shift_ena){
      qReg := (qReg << U(1)).resize(4)
      qReg(0) := io.data
    }.elsewhen(io.count_ena){
      qReg := qReg - U(1)
    }

    io.q := qReg
  }
  noIoPrefix()
}

object counter {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "counter.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new counter().setDefinitionName("top_module"))
  }
}

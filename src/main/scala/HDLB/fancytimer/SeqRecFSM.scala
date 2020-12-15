package HDLB.fancytimer

import spinal.core._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Exams/review2015_fsmseq
class SeqRecFSM extends Component {

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val data = in Bool
    val start_shifting = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val seqRec = new mylib.SeqRec("1101", io.data)
    io.start_shifting := seqRec
  }

  noIoPrefix()
}

object SeqRecFSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "SeqRecFSM.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new SeqRecFSM().setDefinitionName("top_module"))
  }
}

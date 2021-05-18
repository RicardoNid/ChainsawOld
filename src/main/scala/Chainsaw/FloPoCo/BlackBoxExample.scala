package Chainsaw.FloPoCo

import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import xilinx.VivadoFlow


class BlackBoxExample(wIn: Int, constant: Int) extends BlackBox {

  setBlackBoxName(s"IntConstMult_${wIn}_${constant}_F400_uid2")

  val io = new Bundle {
    val clk = in Bool()
    val X = in(QFormatReal(SQ(wIn, 0)))
    val R = in(QFormatReal(SQ(wIn + log2Up(constant), 0)))
  }

  mapCurrentClockDomain(io.clk)

  noIoPrefix()

  addRTLPath(defaultOutputPath)
}

class TopLevelExample(wIn: Int, constant: Int) extends Component{
  val io = new Bundle {
    val X = in(QFormatReal(SQ(wIn, 0)))
    val R = in(QFormatReal(SQ(wIn + log2Up(constant), 0)))
  }

  val SCM = new BlackBoxExample(wIn, constant)
  io.X <> SCM.io.X
  io.R <> SCM.io.R
}

object BlackBoxExample {
  def main(args: Array[String]): Unit = {
    val report = SpinalVerilog(new TopLevelExample(16, 101))
    report.mergeRTLSource("mergeRTL")
    println(report.rtlSourcesPaths)
    VivadoFlow

  }


}



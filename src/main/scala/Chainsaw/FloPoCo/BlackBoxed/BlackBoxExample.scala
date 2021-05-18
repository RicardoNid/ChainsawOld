package Chainsaw.FloPoCo.BlackBoxed

import Chainsaw.FloPoCo.Transplanted.SCM
import Chainsaw.FloPoCo.defaultOutputPath
import Chainsaw._
import spinal.core._


class BlackBoxExample(wIn: Int, constant: Int) extends BlackBox {

  setBlackBoxName(s"IntConstMult_${wIn}_${constant}_F400_uid2")

  val io = new Bundle {
    val clk = in Bool()
    val X = in(QFormatReal(SQ(wIn, 0)))
    val R = out(QFormatReal(SQ(wIn + log2Up(constant), 0)))
  }

  mapCurrentClockDomain(io.clk)

  noIoPrefix()

  addRTLPath(defaultOutputPath)
}

class TopLevelExample(wIn: Int, constant: Int) extends Component {
  val io = new Bundle {
    val X = in(QFormatReal(SQ(wIn, 0)))
    val R = out(QFormatReal(SQ(wIn + log2Up(constant), 0)))
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

    Chainsaw.ChainsawDebug = true

    val genreport = SpinalConfig().generateSystemVerilog(
      new Component {
        val input = in(UIntReal(1 << 14))
        val scm = SCM(input, 101)
        val output = out(scm.implicitValue)
      })

    //    val floreport = VivadoFlow(new TopLevelExample(16, 101),
    //      topModuleName = "SCMBlackBox",
    //      workspacePath = "/home/ltr/IdeaProjects/Chainsaw/synthWorkspace/SCMBlackBox",
    //      force = true).doit()

  }
}



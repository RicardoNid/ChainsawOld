package DSP

import spinal.core._
import xilinx.VivadoFlow //  for digital signal processing

class Division extends Component {
  //  val input0 = in SFix(10 exp, -10 exp)
  //  val input1 = in SFix(10 exp, -10 exp)
  //  val output = out SFix(10 exp, -10 exp)

  val input0 = in SInt (20 bits)
  val input1 = in SInt (20 bits)
  val output = out SInt (20 bits)

  output := RegNext(RegNext(input0) / RegNext(input1))
}

object Division {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new Division)
    val report = VivadoFlow(new Division, "Division", "output/Division", force = true).doit()
    report.printFMax
    report.printArea
  }
}
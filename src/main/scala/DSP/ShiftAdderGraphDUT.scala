package DSP

import DSP.ASSSign._
import breeze.numerics.abs
import spinal.core._
import spinal.lib.{master, slave}
import xilinx.VivadoFlow

//  LUT using SInt
class ShiftAdderGraphDUT(adderGraph: AdderGraph) extends Component with DSPGen {

  val input = slave Flow globalType

  def outputType = SFix(globalType.maxExp + log2Up(adderGraph.valueOfOutputs.map(abs(_)).sum) exp, globalType.minExp exp)

  val output = master Flow Vec(outputType, adderGraph.numberOfOutputs) //  bitWidth can be determined later

  val ret = ShiftAdderGraph(input.payload, adderGraph).implicitValue
  (0 until adderGraph.numberOfOutputs).foreach(i => output.payload(i) := RegNext(ret(i)).truncated)

  output.valid := RegNext(input.valid)
  output.valid.init(False)

  override def delay: Int = 1
}

object ShiftAdderGraphDUT {
  def main(args: Array[String]): Unit = {
    val adderGraph = new AdderGraph()
    adderGraph.addFundamental(1, 1, AOperation(2, SUBNEXT))
    adderGraph.addFundamental(1, 1, AOperation(3, ADD))
    adderGraph.addFundamental(9, 1, AOperation(0, 1, 0, ADD))
    adderGraph.addFundamental(9, 1, AOperation(0, 2, 0, ADD))
    adderGraph.addFundamental(3, 11, AOperation(0, 4, 0, SUBPREV))
    adderGraph.addOutput(9, 0)
    adderGraph.addOutput(11, 2)
    adderGraph.addOutput(13, 4)
    adderGraph.addOutput(173, 1)

    SpinalConfig().generateSystemVerilog(new ShiftAdderGraphDUT(adderGraph))
    val report = VivadoFlow(new ShiftAdderGraphDUT(adderGraph), "ShiftAdderGraph", "output/ShiftAdderGraph", force = true).doit()
    report.printArea
    report.printFMax
  }
}
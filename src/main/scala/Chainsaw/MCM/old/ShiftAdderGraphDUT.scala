package Chainsaw.MCM.old

import Chainsaw.MCM.ASSSign._
import Chainsaw.MCM.{AOperation, AdderGraph}
import Chainsaw.SAGArch._
import Chainsaw.{DSPGen, MySF, SAGArch, globalType}
import breeze.numerics.abs
import spinal.core._
import spinal.lib.{Delay, master, slave}
import xilinx.VivadoFlow

//  LUT using SInt
class ShiftAdderGraphDUT(adderGraph: AdderGraph, sagArch: SAGArch = NORMAL) extends Component with DSPGen {

  val numberOfOutputs = adderGraph.numberOfOutputs
  val input = slave Flow globalType

  def outputType = SFix(globalType.maxExp + log2Up(adderGraph.valueOfOutputs.map(abs(_)).sum) exp, globalType.minExp exp)

  val output = master Flow Vec(outputType, numberOfOutputs) //  bitWidth can be determined later

  val inputReg = RegNext(input.payload)

  sagArch match {
    case SAGArch.NORMAL => {
      val ret = ShiftAdderGraph(inputReg, adderGraph).implicitValue
      (0 until numberOfOutputs).foreach(i => output.payload(i) := RegNext(ret(i)).truncated)
    }
    case SAGArch.RAW => {
      val coefficients = adderGraph.valueOfOutputs.toArray.sorted
      (0 until numberOfOutputs).foreach(i => output.payload(i) := RegNext(inputReg * MySF(coefficients(i).toDouble)).addAttribute("use_dsp = \"no\""))
    }
  }

  output.valid := Delay(input.valid, 2, False)
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

    //    SpinalConfig().generateSystemVerilog(new ShiftAdderGraphDUT(adderGraph, RAW))

    val report = VivadoFlow(new ShiftAdderGraphDUT(adderGraph), "ShiftAdderGraph", "output/ShiftAdderGraph", force = true).doit()
    val reportRAW = VivadoFlow(new ShiftAdderGraphDUT(adderGraph, RAW), "ShiftAdderGraphRAW", "output/ShiftAdderGraphRAW", force = true).doit()

    println("RAG Result")
    report.printArea
    report.printFMax
    println("RAW Vivado Result")
    reportRAW.printArea
    reportRAW.printFMax
  }
}
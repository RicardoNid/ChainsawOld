package DSP

import spinal.core._
import spinal.lib.{master, slave}

//  LUT using SInt
class ShiftAdderGraphDUT(adderGraph: AdderGraph) extends Component with DSPGen {

  val input = slave Flow globalType
  val output = master Flow globalType //  bitWidth can be determined later

  output.payload := ShiftAdderGraph(input.payload, adderGraph).implicitValue.truncated
  output.valid := RegNext(input.valid)
  output.valid.init(False)

  override def delay: Int = 1
}

object ShiftAdderGraphDUT {
  def main(args: Array[String]): Unit = {
    val adderGraph = new AdderGraph()
    adderGraph.addFundamental(1, 1, 3)
    adderGraph.addFundamental(1, 3, 13)

    SpinalConfig().generateSystemVerilog(new ShiftAdderGraphDUT(adderGraph))
  }
}
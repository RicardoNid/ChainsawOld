package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import Chainsaw.DSP.DAS._
import spinal.lib.eda.altera.QuartusFlow

class QuartusFlowTest extends AnyFlatSpec {

  it should "synthesis and report" in {
    new QuartusFlow(AngleCordic()).impl()
  }

}

// found some in lib.eda.altera !!!!!!!!
object QuartusFlowSTDTest {
  def main(args: Array[String]) {
    SpinalVerilog(AngleCordic())
    val report = eda.altera.QuartusFlow(
      quartusPath     = "/tools/quartus/bin",
      workspacePath   = "/home/xdh/IdeaProjects/Chainsaw/quartusWorkspace",
      toplevelPath    = "/home/xdh/IdeaProjects/Chainsaw/DivideCordic.v",
      family          = "Cyclone V",
      device          = "5CSEMA5F31C6",
      frequencyTarget = 200 MHz
    )
    println("----------------------------------------------")
    println(report.getArea())
    println(report.getFMax())
  }
}

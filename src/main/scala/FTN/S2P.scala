package FTN

import FTN.S2PMode.INTERLEAVE
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import xilinx.VivadoFlow //  for digital signal processing

object S2PMode extends Enumeration {
  type S2PMode = Value
  val INDEPENDENT, INTERLEAVE = Value
}

import FTN.S2PMode.S2PMode

class S2P(widthIn: Int, widthOut: Int, mode: S2PMode) extends Component {
  require(widthOut >= widthIn && widthOut % widthIn == 0)

  val length = widthOut / widthIn

  val input = slave Stream Bits(widthIn bits)
  val output = master Stream Bits(widthOut bits)

  val srls = input.payload.asBools.map(History(_, length, True))

  mode match {
    // interleaving depth = widthIn
    case FTN.S2PMode.INDEPENDENT => for (wi <- 0 until widthIn; l <- 0 until length) output.payload(wi * length + l) := srls(wi)(l)
    case FTN.S2PMode.INTERLEAVE => for (wi <- 0 until widthIn; l <- 0 until length) output.payload(wi + l * widthIn) := srls(wi)(l)
  }

  input.ready := True
  output.valid := Delay(input.valid, length, init = False)
}

object S2P {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new S2P(4, 16, INTERLEAVE)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.input.valid #= false
      dut.clockDomain.waitSampling()
      dut.input.valid #= true
      dut.input.payload #= BigInt("0010", 2)
      dut.clockDomain.waitSampling()
      dut.input.payload #= BigInt("1011", 2)
      dut.clockDomain.waitSampling()
      dut.input.payload #= BigInt("1101", 2)
      dut.clockDomain.waitSampling()
      dut.input.payload #= BigInt("0011", 2)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling(2)
    }
  }

  val report = VivadoFlow(design = new S2P(64, 512, INTERLEAVE), topModuleName = "FTNS2P", workspacePath = "output/FTN/S2P").doit()
}

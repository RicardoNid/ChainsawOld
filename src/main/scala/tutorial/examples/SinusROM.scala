package tutorial.examples

import spinal.core._
import spinal.lib._
import spinal.core.sim._

class SinusROM(resolutionWidth: Int, sampleCount: Int) extends Component {
  val io = new Bundle {
    val sin = out SInt (resolutionWidth bits)
    val sinFiltered = out SInt (resolutionWidth bits)
  }

  def sinTable = Range(0, sampleCount).
    map(i => Math.sin(2 * Math.PI * i / sampleCount)).
    map(sinValue => S((sinValue * ((1 << resolutionWidth) / 2 - 1)).toInt, resolutionWidth bits))

  val rom = Mem(SInt(resolutionWidth bits), initialContent = sinTable)
  val phase = Reg(UInt(log2Up(sampleCount) bits)) init(0)
  phase := phase + 1

  io.sin := rom.readSync(phase)
  io.sinFiltered := RegNext(io.sinFiltered - (io.sinFiltered >> 5) + (io.sin >> 5)) init(0)
}

object SinusROM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new SinusROM(12, 64))
  }
}

object testSinusROM {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new SinusROM(12, 64)).
    doSimUntilVoid { dut =>
      val clockThread = fork{
        dut.clockDomain.risingEdge()
        while (true){
          dut.clockDomain.clockToggle()
          sleep(1)
        }
      }
      val mainThread = fork{
        for(i <- 0 until 1000){
          sleep(2)
        }
        simSuccess()
      }
    }
  }
}

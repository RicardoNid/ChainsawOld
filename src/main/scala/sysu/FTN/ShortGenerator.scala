package sysu.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import sysu.FTN.OFDM.{OFDMCDConfig, dataType, shortTrainingSequence}

class ShortGenerator extends Component {
  val io = new Bundle {
    val clkFFT = in Bool
    val shortRstn = in Bool
    val shortAck = in Bool

    val shortRe = out(dataType)
    val shortIm = out(dataType)
    val shortIndex = out UInt (log2Up(180) bits)
    val shortValid = out Bool
  }

  val mainCD = ClockDomain(
    clock = io.clkFFT,
    reset = io.shortRstn,
    config = OFDMCDConfig)

  val mainArea = new ClockingArea(mainCD) {

    val ROMCounter = Counter(0 until 16, io.shortAck)
    val indexCounter = Counter(0 until 200, io.shortAck)
    val shortROM = Mem(dataType, shortTrainingSequence)

    def whenNotWorking = {
      io.shortRe := 0
      io.shortIm := 0
      io.shortIndex := 0
      io.shortValid := False

      ROMCounter.clear()
      indexCounter.clear()
    }

    when(!io.shortRstn) {
      whenNotWorking
    }
      .elsewhen(io.shortAck) {
        io.shortRe := shortROM(ROMCounter.value.resized)
        io.shortIm := shortROM((ROMCounter.value + U(16)).resized)
        io.shortIndex := indexCounter.value
        io.shortValid := True
      }
      .otherwise {
        whenNotWorking
      }
  }

}

object ShortGenerator {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new ShortGenerator)
  }
}

object testShortGenerator {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new ShortGenerator).
      doSimUntilVoid { dut =>

        val clockThread = fork {
          dut.mainCD.forkStimulus(period = period)
        }

        val mainThread = fork {
          sleep(period / 2)
          dut.io.shortAck #= false
          sleep(period)
          dut.io.shortAck #= true
          sleep(500 * period)
          dut.io.shortAck #= false
          sleep(period)
          dut.io.shortAck #= true
          sleep(500 * period)
          simSuccess()
        }
      }
  }
}

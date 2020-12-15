package examples.stream

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class BasicStream(depth: Int) extends Component {

  def word = Bits(8 bits)

  val io = new Bundle {
    val input = slave Stream (word)
    val output = master Stream (word)
    val inputFire = out Bool
    val outputFire = out Bool
  }

  val innerFifo = StreamFifo(
    dataType = word,
    depth = depth
  )

  innerFifo.io.push << io.input
  innerFifo.io.pop >> io.output

  io.inputFire := io.input.fire
  io.outputFire := io.output.fire
}

object BasicStream {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = SystemVerilog,
      targetDirectory = "C:/Users/lsfan/Documents/GitHub/FTN/FTN.srcs"
    ).generate(new BasicStream(8))
  }
}

object testBasicStream {

  val randGen = new Random(42)
  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new BasicStream(8)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }

        val mainThread = fork {

          def randInput = randGen.nextInt(1 << 8)
          // 模式1,一直读写
          dut.io.input.valid #= true
          dut.io.output.ready #= true
          sleep(17 * period)
          sleep(period / 2)
          for (i <- 0 until 100) {
            dut.io.input.payload #= randInput
            sleep(period)
          }
          // 模式2,只写不读
          dut.io.input.valid #= true
          dut.io.output.ready #= false
          for (i <- 0 until 100) {
            dut.io.input.payload #= randInput
            sleep(period)
          }
          // 模式3,只读不写
          dut.io.input.valid #= false
          dut.io.output.ready #= true
          for (i <- 0 until 100) {
            dut.io.input.payload #= randInput
            sleep(period)
          }
          // 模式4,深度范围内,交替读写
          for (i <- 0 until 25; j <- 0 until 4) {
            dut.io.input.valid #= (if (j != 3) true else false) // 前三周期写
            dut.io.output.ready #= (if (j != 0) true else false) // 后三周期读
            dut.io.input.payload #= randInput
            sleep(period)
          }
          simSuccess()
        }
      }
  }
}

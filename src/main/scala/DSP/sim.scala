package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._

object sim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.allOptimisation.compile(StreamFifo(Bits(8 bits), 2)).doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)

        fork {
          while (true) {
            dut.io.push.valid #= true
            dut.io.push.payload.randomize()
            dut.clockDomain.waitSampling()
            if (dut.io.push.valid.toBoolean && dut.io.push.ready.toBoolean) {
              printf("push %d\n", dut.io.push.payload.toInt)
            }
            if (!dut.io.push.ready.toBoolean) {
              printf("FIFO is full\n")
            }
          }
        }

        fork {
          for (i <- 0 until 100) {
            dut.clockDomain.waitSampling()
            dut.io.pop.ready #= true
            if (dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean) {
              printf("pop %d\n", dut.io.pop.payload.toInt)
            }
            if (!dut.io.pop.valid.toBoolean) {
              printf("FIFO is empty\n")
            }
          }
          simSuccess()
        }
    }
  }
}
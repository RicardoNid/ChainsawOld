package DSP

import spinal.core.sim._

object simMine {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.allOptimisation.compile(SFIFO(8, 2)).doSimUntilVoid {
      dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.data_in.w_req #= false
        dut.io.data_out.r_req #= false

        fork {
          while (true) {
            dut.io.data_in.w_req #= true
            dut.io.data_in.data.randomize()
            dut.clockDomain.waitSampling()
            if (dut.io.data_in.w_req.toBoolean && dut.io.data_in.r_req.toBoolean) {
              printf("pushing %d\n", dut.io.data_in.data.toInt)
            }
            if (!dut.io.data_in.r_req.toBoolean) {
              printf("FIFO is full\n")
            }
          }
        }

        fork {
          for (i <- 0 until 100) {
            dut.clockDomain.waitSampling()
            dut.io.data_out.r_req #= true
            if (dut.io.data_out.r_req.toBoolean && dut.io.data_out.w_req.toBoolean) {
              printf("poping %d\n", dut.io.data_out.data.toInt)
            }
            if (!dut.io.data_out.w_req.toBoolean) {
              printf("FIFO is empty\n")
            }
          }
          simSuccess()
        }
    }
  }
}
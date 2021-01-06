package sysu.util

import breeze.numerics.ceil
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import sysu.Opt.CounterParam
import sysu.xilinx._
import sysu.util._

class BetterCounter(counterParam: CounterParam) extends Component {

  import counterParam._

  val valueMax = (ceil(period / width.toDouble) - 1).toInt * step
  val bitWidth = log2Up(valueMax + 1)

  val io = new Bundle {
    val output = out UInt (bitWidth bits)
  }
  val outputReg = RegInit(U(0, bitWidth bits))

  if (width != 0) {
    val innerCounter = Counter(width, True)
    val periodCounter = Counter(period, True)

    when(periodCounter.willOverflow){
      outputReg := U(0)
      innerCounter.clear()
    }
      .elsewhen(innerCounter.willOverflow)(outputReg := outputReg + step)
  }
  else {
    val periodCounter = Counter(period, True)
    when(periodCounter.willOverflow)(outputReg := U(0)).
      otherwise(outputReg := outputReg + step)
  }
  io.output := outputReg
}

object testBetterCounter {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new BetterCounter(CounterParam(7, 2, 5)))
      .doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}

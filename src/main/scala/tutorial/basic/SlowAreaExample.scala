package tutorial.basic

import spinal.core._
import spinal.lib._
import spinal.core.sim._

class SlowAreaExample extends Component {

  val io = new Bundle {
    val o1, o2 = out UInt (4 bits)
  }

  //  val counter1, counter2 = Counter(10)
  val counter1 = Counter(10)
  counter1.increment()

  io.o1 := counter1.value

  new SlowArea(4) {
    val counter2 = Counter(10)
    counter2.increment()
    io.o2 := counter2.value
  }
}

object SlowAreaExample {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new SlowAreaExample)
  }
}

object testSlowAreaExampl {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new SlowAreaExample)
      .doSimUntilVoid { dut =>

        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = 2)
        }
        sleep(100)
        simSuccess()
      }
  }
}
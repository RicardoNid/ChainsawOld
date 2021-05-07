package DSP

import spinal.core._
import spinal.core.sim._

class PlayWithReal extends Component {

  val a, b = SReal(IntRange(0, 127))
  val c = SReal(IntRange(0, 256))

  c := a + b

  in(a, b)
  out(c)
}

object PlayWithReal {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new PlayWithReal) doSim { dut =>
      dut.a.raw #= 63
      dut.b.raw #= 65
      println(dut.c.raw.toInt)
    }
  }
}


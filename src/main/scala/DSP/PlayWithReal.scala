package DSP

import spinal.core._
import spinal.core.sim._

import scala.util.Random

class PlayWithReal extends Component {


  val a = SReal(IntRange(0, 10))
  val af = SFix(4 exp, 0 exp)
  val b = SReal(IntRange(0, 10))
  val bf = SFix(4 exp, 0 exp)
  val c = SReal(IntRange(0, 20))
  val cf = SFix(5 exp, 0 exp)

  c := a + b
  cf := af +^ bf

  in(a, b, af, bf)
  out(c, cf)
}

object PlayWithReal {

  val r = new Random()

  val uppers = (0 until 10).map(_ => r.nextInt(1023))


  def main(args: Array[String]): Unit = {

    SimConfig.withWave.compile(new PlayWithReal) doSim { dut =>
      dut.a #= 8
      dut.af.raw #= 8
      dut.b #= 9
      dut.bf.raw #= 9
      sleep(1)
      println(dut.c.raw.toInt)
      println(dut.cf.raw.toInt)
    }
  }
}


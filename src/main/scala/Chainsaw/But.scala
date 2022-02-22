package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class But {

  def butterfly[T](dataIn: Seq[T]) = Seq(dataIn(1), dataIn(0))

  def build[T](dataIn: Seq[T]): Seq[T] = {
    if (dataIn.size == 2) butterfly(Seq(dataIn(0), dataIn(1)))
    else {
      val n = dataIn.size
      val afterPre = dataIn.take(n / 2).zip(dataIn.takeRight(n / 2)).map { case (a, b) => butterfly(Seq(a, b)) }
      val reordered = afterPre.map(_ (0)) ++ afterPre.map(_ (1))
      build(reordered.take(n / 2)) ++ build(reordered.takeRight(n / 2))
    }
  }
}

case class Literal() extends Component {

  import scala.math.cos

  val w0 = SF(cos(1.5), 3 exp, -14 exp)
  out(w0)
}

object Literal extends App {

  SpinalSimConfig().withFstWave.compile(Literal()).doSim { dut =>
    sleep(10)
  }

}

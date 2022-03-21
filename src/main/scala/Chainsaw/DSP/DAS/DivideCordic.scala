package Chainsaw.DSP.DAS

import spinal.core._
import spinal.lib._

object CordicConfig {
  val iter = 16
}

case class DivideCordic() extends Component {
  val yIn  = in(Para.dataType())
  val zOut = out(Para.dataType())

  val y = Vec(Reg(Para.dataType()) init (0), CordicConfig.iter)
  val d = y.map(_.toSInt).map(_.sign).map(~_)
  val z = Vec(Reg(Para.dataType()) init (0), CordicConfig.iter)

  val theta = Range(0, CordicConfig.iter).map(1 / BigInt(2).pow(_).toDouble).map { i =>
    val ret = Para.dataType()
    ret := i
    ret
  } // theta(i) = 2^{-i}

  val x = Para.dataType()
  x := scala.math.Pi
  // connect y
  y.head := yIn
  Range(1, CordicConfig.iter).foreach { i =>
    when(d(i - 1)) {
      y(i) := y(i - 1) - (x >>| (i - 1))
    } otherwise {
      y(i) := y(i - 1) + (x >>| (i - 1))
    }
  }
  // connect z
  z.head := 0
  Range(1, CordicConfig.iter).foreach { i =>
    when(d(i - 1)) {
      z(i) := z(i - 1) + theta(i - 1)
    } otherwise {
      z(i) := z(i - 1) - theta(i - 1)
    }
  }

  zOut := z.last
}

package Chainsaw.DSP.DAS

import spinal.core._
import spinal.lib._
import scala.math._

object CordicConfig {
  val iter = 16
}

case class AngleCordic() extends Component {
  val io = new Bundle {
    val xIn, yIn = in(Para.dataType())
    val zOut     = out(Para.dataType())
  }

  val x, y = Vec(Reg(Para.dataType()) init (0), CordicConfig.iter)
  val d    = x.zip(y).map { case (xr, yr) => xr.toSInt.sign ^ yr.toSInt.sign }.map(~_) //y.map(_.toSInt).map(_.sign).map(~_)
  val z    = Vec(Reg(Para.dataType()) init (0), CordicConfig.iter)

  // theta(i) = arctan(2^{-i})
  val theta = Range(0, CordicConfig.iter).map(1 / BigInt(2).pow(_).toDouble).map(atan(_)).map { i =>
    val ret = Para.dataType()
    ret := i
    ret
  }
  x.head := io.xIn
  x.tail.zipWithIndex.foreach { case (s, i) => when(d(i))(x(i + 1) := x(i) + (y(i) >>| i)) otherwise (x(i + 1) := x(i) - (y(i) >>| i)) }

  y.head := io.yIn
  y.tail.zipWithIndex.foreach { case (s, i) => when(d(i))(y(i + 1) := y(i) - (x(i) >>| i)) otherwise (y(i + 1) := y(i) + (x(i) >>| i)) }

  z.head := 0
  z.tail.zipWithIndex.foreach { case (s, i) => when(d(i))(z(i + 1) := z(i) + theta(i)) otherwise (z(i + 1) := z(i) - theta(i)) }

  io.zOut := z.last
}

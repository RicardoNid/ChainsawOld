package Chainsaw.matlabIO

import breeze.linalg.{norm, _}
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import breeze.generic.UFunc._
import breeze.generic.UFunc


case class FixedCast() {
  def cast(from: Fixed, to: Fixed) = {

    val overflow = from.w > to.w
    val rounding = from.f > to.f

    val high = if (overflow) from.storedInt >> from.f
    else from.storedInt >> from.f

    val low = if (rounding) from.storedInt % (1 << from.f)
    else from.storedInt % (1 << from.f)

    Fixed(to.w, to.f, (high << to.f) + low)
  }
}


case class Fixed(w: Int, f: Int, storedInt: BigInt)


object testFixed {
  def main(args: Array[String]): Unit = {
    val a = Fixed(32, 16, BigInt(1))
    val b = Fixed(17, 16, BigInt(1))
    val cast = FixedCast()
    println(cast.cast(a, b))
  }
}

package Chainsaw.fastAlgos

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import scala.annotation.tailrec

object Gray {

  def grayTable(width: Int): Seq[Int] = {

    // this is how gray code grow
    def grow(data: Seq[Int]) = data ++ data.reverse.map(_ + data.length)

    if (width == 1) Seq(0, 1) // initial
    else grow(grayTable(width - 1)) // grow
  }

  def toGray(bin: Int, width: Int) = grayTable(width)(bin)

  def fromGray(gray: Int, width: Int) = grayTable(width).indexOf(gray)

  def main(args: Array[String]): Unit = {
    val grays = (0 until 8).map(toGray(_, 3))
    val bins = grays.map(fromGray(_, 3))

    println(grays)
    println(bins)
  }
}

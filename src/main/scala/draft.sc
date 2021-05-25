implicit val ulp = 0.001

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real


import scala.math.{ceil, log, max, pow}

def log2(value: Double) = log(value) / log(2.0)
def log2Up(value: Double) = ceil(log2(value)).toInt
def maxExp(max: Int, min: Int) = log2Up(pow(2, max) + pow(2, min))


(0 until 100).map(max => maxExp(max, 0))

val matrix = Array.tabulate(50, 50) { case (max, min) =>
  val maxValue = max - 25
  val minValue = min - 25
  if (maxValue <= minValue) " "
  else if (maxExp(maxValue, minValue) != maxValue + 1) "x"
  else "o"
}

println(matrix.zipWithIndex
  .map{ case (strings, i) => (i-25).toString.padTo(4, ' ') + strings.mkString("")}
  .mkString("\n"))


SQ(16, 5).nonFraction
SQ(16, 5).fraction
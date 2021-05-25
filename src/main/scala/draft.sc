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
def maxExp(max: Int, min: Int) = log2Up((1 << max) + pow(2, min))


(0 until 100).map(max => maxExp(max, 0))

Array.tabulate(64, 64)(_ + _).map(_.mkString(" ")).mkString("\n")



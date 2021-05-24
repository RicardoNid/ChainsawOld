import scala.math.{ceil, log, max, pow}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

def log2(value: Double) = {
  require(value >= 0.0, s"operand of log2 should >= 0.0, fix it")
  if (value == 0.0) 0.0 else log(value) / log(2.0)
}

def log2Up(value: Double) = ceil(log2(value)).toInt

def getMaxExp(implicit ulp: Double) = {
  println(ulp)
  def bitsForBound(bound: Double) = if (bound >= 0.0) log2Up(bound + ulp) else log2Up(-bound)

  max(bitsForBound(256.0), bitsForBound(-256.0))
}

getMaxExp(pow(2, -45))
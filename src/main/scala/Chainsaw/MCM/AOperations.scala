package Chainsaw.MCM

import Chainsaw.printlnWhenDebug
import spinal.core._

import scala.collection.mutable

object AOpSign extends Enumeration {
  type AOpSign = Value
  val ADD, SUBNEXT, SUBPREV = Value
}

import Chainsaw.MCM.AOpSign._

case class AOpConfig(shiftLeftLeft: Int, shiftLeftRight: Int, shiftRight: Int, aOpSign: AOpSign)

object AOperations {
  
  val AOpHardware = (left: Real, right: Real, config: AOpConfig) => {
    import config._
    printlnWhenDebug(config)
    val shiftedLeft = left << shiftLeftLeft
    val shiftedRight = right << shiftLeftRight
    printlnWhenDebug(shiftedLeft)
    printlnWhenDebug(shiftedLeft.realInfo.interval)
    printlnWhenDebug(shiftedRight)
    printlnWhenDebug(shiftedRight.realInfo.interval)
    val sum = config.aOpSign match {
      case ADD => shiftedLeft + shiftedRight
      case SUBNEXT => shiftedLeft - shiftedRight
      case SUBPREV => shiftedRight - shiftedLeft
    }
    printlnWhenDebug(sum)
    printlnWhenDebug(sum.realInfo.interval)
    val ret = sum >> shiftRight
    printlnWhenDebug(ret)
    printlnWhenDebug(ret.realInfo.interval)
    ret
  }

  /** Get corresponding positive odd fundamental of n
   *
   * @example getPOF(-24) = 3, as 24 = 3 << 3
   */
  def getPOF(n: Int) = {
    require(n != 0)
    var ret = n.abs //  positive
    while (ret % 2 == 0) ret /= 2 // odd
    ret
  }

  def AReverse(sum: Int, left: Int, right: Int): AOpConfig = {
    require(sum > 0 && left > 0 && right > 0 && sum % 2 != 0 && left % 2 != 0 && right % 2 != 0, s"$left, $right, $sum  fundamentals should be preprocessed into positive odd")
    printlnWhenDebug(s"rebuilding $left,$right -> $sum")
    val cond1 = sum == getPOF(left + right)
    val cond2 = (left > right) && (sum == getPOF(left - right))
    val cond3 = (left < right) && (sum == getPOF(right - left))
    val cond4 = (sum - left > 0) && ((sum - left) % right == 0) && isPow2((sum - left) / right) // w = 2 << i * v + u
    val cond5 = (sum - left < 0) && (-(sum - left) % right == 0) && isPow2((left - sum) / right) // w = u - 2 << i * v
    val cond6 = (sum + left > 0) && ((left + sum) % right == 0) && isPow2((left + sum) / right) // w = 2 << i * v - u
    val cond7 = (sum - right > 0) && ((sum - right) % left == 0) && isPow2((sum - right) / left) // w = 2 << i * u + v
    val cond8 = (sum - right < 0) && (-(sum - right) % left == 0) && isPow2((right - sum) / left) // w = v - 2 << i * u
    val cond9 = (sum + right > 0) && ((right + sum) % left == 0) && isPow2((right + sum) / left) // w = 2 << i * u - v

    if (cond1) AOpConfig(0, 0, log2Up((left + right) / sum), ADD)
    else if (cond2) AOpConfig(0, 0, log2Up((left + right) / sum), SUBNEXT)
    else if (cond3) AOpConfig(0, 0, log2Up((left + right) / sum), SUBPREV)
    else if (cond4) AOpConfig(0, log2Up((sum - left) / right), 0, ADD)
    else if (cond5) AOpConfig(0, log2Up((left - sum) / right), 0, SUBNEXT)
    else if (cond6) AOpConfig(0, log2Up((sum + left) / right), 0, SUBPREV)
    else if (cond7) AOpConfig(log2Up((sum - right) / left), 0, 0, ADD)
    else if (cond8) AOpConfig(log2Up((right - sum) / left), 0, 0, SUBPREV)
    else if (cond9) AOpConfig(log2Up((sum + right) / left), 0, 0, SUBNEXT)
    else AOpConfig(0, 0, 0, ADD)
  }

  implicit class AImplicit(u: Int) {

    def AOp(v: Int)(implicit max: Int): mutable.Set[Int] = {
      require(u > 0 && v > 0 && u % 2 != 0 && v % 2 != 0) //  positive odd fundamentals
      val ret = mutable.Set[Int]() //  reachable coefficients
      var exp = 1
      var continue = true
      while (continue) { //  situation 1 & 2, j = 0, k > 0 or j >0, k = 0
        val cands = Array( //  1 << exp stands for 2^i
          (1 << exp) * u + v, (1 << exp) * u - v, v - (1 << exp) * u,
          (1 << exp) * v + u, (1 << exp) * v - u, u - (1 << exp) * v)
        val validCand = cands.filter(cand => cand > 0 && cand <= max)
        validCand.foreach(ret += _)
        continue = validCand.map(_ * 2).exists(_ < max)
        exp += 1
      }
      ret += getPOF(u + v) //  situation 3, j = k < 0
      if (u != v) ret += getPOF(if (u > v) u - v else v - u)
      ret
    }
  }
}

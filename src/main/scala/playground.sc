import DSP.RAGn.{AOperation, getPositiveOddFundamental}
import spinal.core.log2Up
//import DSP.AOperation


def AReverse(w: Int, u: Int, v: Int) = {
  require(w > 0 && u > 0 && v > 0 && w % 2 != 0 && u % 2 != 0 && v % 2 != 0, s"$u, $v, $w  fundamentals should be preprocessed into positive odd")
  if (w == getPositiveOddFundamental(u + v)) Some((0, 0, log2Up((u + v) / w), true))
  else {
    var quit = false
    var found = false
    var result = (0, 0, 0, true)
    var exp = 1
    while (!found && !quit) {
      val cands = Array( //  1 << exp stands for 2^i
        (1 << exp) * u + v, (1 << exp) * u - v,
        (1 << exp) * v + u, (1 << exp) * v - u)
      val index = cands.indexOf(w)
      if (index >= 0) {
        found = true
        index match {
          case 0 => result = (exp, 0, 0, true)
          case 1 => result = (exp, 0, 0, false)
          case 2 => result = (0, exp, 0, true)
          case 3 => result = (0, exp, 0, false)
        }
      }
      else exp += 1
      if (cands.forall(_ > w)) quit = true
    }
    if (found) Some(result)
    else None
  }
}

import scala.util.Random

val r = new Random()

println(AOperation(65, 17, 16384).mkString(" "))

//(0 until 100).map { _ =>
//  val u = getPositiveOddFundamental(r.nextInt(200))
//  val v = getPositiveOddFundamental(r.nextInt(200))
//  println(s"$u, $v")
AOperation(3, 189, 16384).foreach { i =>
  println(i)
  AReverse(i, u, v).get
}
}
//println("success!")

AOperation(3, 189, 16384)
AReverse(3021, 3, 189)




























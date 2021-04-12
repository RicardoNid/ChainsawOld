import DSP.AConfigVector
import DSP.RAGn._
import spinal.core._

def AReverse(w: Int, u: Int, v: Int) = {
  require(w > 0 && u > 0 && v > 0 && w % 2 != 0 && u % 2 != 0 && v % 2 != 0, s"$u, $v, $w  fundamentals should be preprocessed into positive odd")
  if (w == getPositiveOddFundamental(u + v)) Some(AConfigVector(0, 0, log2Up((u + v) / w), true))
  else if (u != v) {
    val diff = if (u > v) u - v else v - u
    if (w == getPositiveOddFundamental(diff)) Some(AConfigVector(0, 0, log2Up(diff), true))
  }
  else {
    var quit = false
    var found = false
    var result = AConfigVector(0, 0, 0, true)
    var exp = 1
    while (!found && !quit) {
      val cands = Array( //  1 << exp stands for 2^i
        (1 << exp) * u + v, (1 << exp) * u - v,
        (1 << exp) * v + u, (1 << exp) * v - u)
      val index = cands.indexOf(w)
      if (index >= 0) {
        found = true
        index match {
          case 0 => result = AConfigVector(exp, 0, 0, true)
          case 1 => result = AConfigVector(exp, 0, 0, false)
          case 2 => result = AConfigVector(0, exp, 0, true)
          case 3 => result = AConfigVector(0, exp, 0, false)
        }
      }
      else exp += 1
      if (cands.forall(_ > w)) quit = true
    }
    if (found) Some(result)
    else None
  }
}

AReverse(11, 13, 9)






























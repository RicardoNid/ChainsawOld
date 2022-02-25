import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

def cmult(a: BComplex, b: BComplex) = a * b

def cmultInDetails(a: BComplex, b: BComplex) = {

  val ar = a.real
  val ai = a.imag
  val br = b.real
  val bi = b.imag

  val m = (br + bi) * ar
  val l = (ai - ar) * br
  val r = (ai + ar) * bi

  BComplex(m - r, l + m)
}

val a  = BComplex(1.0,-1.0)
val b  = BComplex(2.0,-2.0)

// generate random complex numbers...

assert(cmult(a,b) == cmultInDetails(a,b))
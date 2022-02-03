package Chainsaw.DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

// fully pipelined 3-DSP CMult, using 3 DSPs and extra
case class CMult(hardType: HardType[ComplexNumber]) extends Component {

  val a, b = in(hardType())

  val ar = a.real
  val ai = a.imag
  val br = b.real
  val bi = b.imag

  // mult0
  val mid = ((br.delay(1) +^ bi.delay(1)).delay(1) * ar.delay(2)).delay(2)
  // mult1
  val pr = (mid.delay(1) + ((ai.delay(3) -^ ar.delay(3)).delay(1) * br.delay(4)).delay(1)).delay(1)
  // mult2
  val pi = (mid.delay(1) - ((ar.delay(3) +^ ai.delay(3)).delay(1) * bi.delay(4)).delay(1)).delay(1)

  val realOut = out(pr)
  val imagOut = out(pi)
}

object CMult {
  def main(args: Array[String]): Unit = {
    val hardType = HardType(ComplexNumber(SFix(2 exp, -13 exp)))
    VivadoSynth(CMult(hardType))
  }
}

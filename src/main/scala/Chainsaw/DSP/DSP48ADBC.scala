package Chainsaw.DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.language.postfixOps

// inference of (A+D)*B+C
case class DSP48ADBC() extends Component {

  val a,b,c,d = in SFix(2 exp, -15 exp)

  val ret = (((a +^ d).d(1) * b.d(1)).d(1) + c.d(2)).d(1)
  out(ret)
}

object DSP48ADBC {
  def main(args: Array[String]): Unit = {
    VivadoSynth(DSP48ADBC())
  }
}

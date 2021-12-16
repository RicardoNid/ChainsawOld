package Chainsaw.comm.channelEqualizer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object Compare {

  case class Adder18() extends Component {
    val x, y = in SInt (18 bits)
    val ret = out(x + y)
  }

  case class MUX18_N(N:Int) extends Component {
    val dataIns = Seq.fill(N)(in SInt (18 bits))
    val sel = in Bits(N bits)
    val ret = out(MuxOH(sel, dataIns))
  }

  case class Mult18() extends Component {
    val x, y = in SInt (18 bits)
    val ret = out(x * y).resize(18)
    ret.addAttribute("use_dsp","no")
  }

  def main(args: Array[String]): Unit = {
    VivadoSynth(Adder18())
    (2 until 10).foreach(i => VivadoSynth(MUX18_N(i)))
    VivadoSynth(Mult18())
  }

}

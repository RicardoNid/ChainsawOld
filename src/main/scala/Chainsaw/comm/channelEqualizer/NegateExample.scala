package Chainsaw.comm.channelEqualizer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class NegateExample() extends Component {
  val dataIn = in SInt (18 bits)
  val dataOut = out(-dataIn)
}

case class MinusExample() extends Component {
  val x, y = in SInt (18 bits)
  val dataOut = out(x - y)
}

object testNM{
  def main(args: Array[String]): Unit = {
    VivadoSynth(NegateExample()) // 17 LUT
    VivadoSynth(MinusExample()) // 18 LUT, same timing
  }
}
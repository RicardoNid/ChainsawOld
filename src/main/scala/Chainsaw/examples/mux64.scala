package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class mux64() extends Component {

  val dataIn  = in UInt (6 bits)
  val dataOut = out Bits (64 bits)

  val regs = RegInit(B(0, 64 bits))
  regs(dataIn) := True
  dataOut      := regs
}

object mux64 {
  def main(args: Array[String]): Unit = {
    VivadoSynth(mux64())
  }
}

package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

class ZyboWrapper extends Component {

  val sw  = in Bits (4 bits)
  val btn = in Bits (3 bits)
  val led = out Bits (4 bits)
  led := B"0000"
  led.allowOverride

}

case class ZyboDesign0() extends ZyboWrapper {

  led := RegNext(sw)
  led.addAttribute("mark_debug")

}

object ZyboDesign0 {
  def main(args: Array[String]): Unit = {
    GenRTL(ZyboDesign0())
    VivadoSynth(ZyboDesign0())
  }
}

case class ZyboDesign1() extends ZyboWrapper {

  led := RegNext(sw)

}

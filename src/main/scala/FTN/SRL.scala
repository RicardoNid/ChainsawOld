package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class SRL extends Component {
  val input = in Bool()
  val srl = History(input, 10)
  val output = out(srl.last)
}

object SRL extends App {
  VivadoSynth(new SRL)
}

package FTN

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bram._
import spinal.lib.fsm._

class PIPOMem(dataWidth: Int, addressWidth: Int) extends Component {

  val input = slave(BRAM(BRAMConfig(dataWidth, addressWidth)))
  val output = master
  val interchange = in Bool()


  val fsm = new StateMachine

}

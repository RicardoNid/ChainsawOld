package Chainsaw.lfxWorkspace

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class SwitchBack() extends Component {

  val state = in Bits (4 bits)
  val output = out UInt (3 bits)

  val outputList = Seq(0, 1, 4, 5)


  switch(True) {
    (0 until 4).foreach { i =>
      is(state(i))(output := U(outputList(i)))
    }
    default(output := U(0))
  }

}

object SwitchBack extends App {
  GenRTL(SwitchBack())
}

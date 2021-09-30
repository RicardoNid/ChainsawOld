package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class Genrtl extends Component {

  val dataIn = in UInt(4 bits)
  val dataOut = out UInt(4 bits)


  dataOut := dataIn



}

object Genrtl {

}

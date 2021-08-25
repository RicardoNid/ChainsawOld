package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

class ClockDomainExample extends Component {
  val dataIn = in UInt(4 bits)
  val yourReg = out(Reg(UInt(4 bits)))
  yourReg.init(U(0, 4 bits))
  yourReg := dataIn
  import spinal.core.Data._
  println(InitAssign.asInstanceOf[ResetKind])
}

object ClockDomainExample extends App {
  GenRTL(new ClockDomainExample)
}

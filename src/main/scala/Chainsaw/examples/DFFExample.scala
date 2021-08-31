package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import matlabIO._

case class DFFExample() extends Component {
  val dataIn = in UInt (4 bits)
  val dataOut = out UInt (4 bits)
  val reg0 = createDFF0(UInt(4 bits))
  val reg1 = createDFF1(UInt(4 bits))

  val cross = UInt(4 bits)
  cross.addTag(crossClockDomain)

  reg0.init(0)
  reg1.init(1)

  reg0 := dataIn
  cross := reg0
  reg1 := cross
  dataOut := reg1
}

object DFFExample extends App {
  GenRTL(DFFExample())
}

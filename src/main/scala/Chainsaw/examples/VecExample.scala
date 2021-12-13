package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class VecExample() extends Component {

  val dataIn = in Vec(UInt(3 bits), UInt(4 bits))
  val dataOut = out(dataIn(0) + dataIn(1))

}

object VecExample {
  def main(args: Array[String]): Unit = {
    GenRTL(VecExample())
  }
}

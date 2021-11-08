package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object MultiModuleExample {

  case class Swap() extends Component {
    val dataIn = in Vec(UInt(4 bits), 2)
    val dataOut = out Vec(UInt(4 bits), 2)

    dataOut(0) := dataIn(1)
    dataOut(1) := dataIn(0)
  }

  def main(args: Array[String]): Unit = {
    GenRTL(new Component { // top module
      val dataIn = in Vec(UInt(4 bits), 4)
      val dataOut = out Vec(UInt(4 bits), 4)

      val swap0, swap1 = Swap()

      swap0.dataIn(0) := dataIn(0)
      swap0.dataIn(1) := dataIn(1)
      swap1.dataIn(0) := dataIn(2)
      swap1.dataIn(1) := dataIn(3)

      dataOut(0) := swap0.dataOut(0)
      dataOut(1) := swap0.dataOut(1)
      dataOut(2) := swap1.dataOut(0)
      dataOut(3) := swap1.dataOut(1)
    }, name = "top")
  }

}

package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object MultiModuleExample {

  //  case class Swap() extends Component {
  //    val dataIn = in Vec(UInt(4 bits), 2)
  //    val dataOut = out Vec(UInt(4 bits), 2)
  //
  //    dataOut(0) := dataIn(1)
  //    dataOut(1) := dataIn(0)
  //  }

  def swap = new Component {
    val dataIn  = in Vec (UInt(), 2)
    val dataOut = out Vec (UInt(), 2)

    dataOut(0) := dataIn(1)
    dataOut(1) := dataIn(0)
  }

  def main(args: Array[String]): Unit = {

    GenRTL(
      new Component { // top module
        val dataIn0  = in Vec (UInt(4 bits), 2)
        val dataIn1  = in Vec (UInt(5 bits), 2)
        val dataOut0 = out Vec (UInt(4 bits), 2)
        val dataOut1 = out Vec (UInt(5 bits), 2)

        val swap0, swap1 = swap

        swap0.dataIn(0) := dataIn0(0)
        swap0.dataIn(1) := dataIn0(1)
        swap1.dataIn(0) := dataIn1(0)
        swap1.dataIn(1) := dataIn1(1)

        dataOut0(0) := swap0.dataOut(0)
        dataOut0(1) := swap0.dataOut(1)
        dataOut1(0) := swap1.dataOut(0)
        dataOut1(1) := swap1.dataOut(1)
      },
      name = "diffMultiModule"
    )

    GenRTL(
      new Component { // top module
        val dataIn  = in Vec (UInt(4 bits), 4)
        val dataOut = out Vec (UInt(4 bits), 4)

        val swap0, swap1 = swap

        swap0.dataIn(0) := dataIn(0)
        swap0.dataIn(1) := dataIn(1)
        swap1.dataIn(0) := dataIn(2)
        swap1.dataIn(1) := dataIn(3)

        dataOut(0) := swap0.dataOut(0)
        dataOut(1) := swap0.dataOut(1)
        dataOut(2) := swap1.dataOut(0)
        dataOut(3) := swap1.dataOut(1)
      },
      name = "sameMultiModule"
    )

  }
}

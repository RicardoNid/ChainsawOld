package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class OrderExample() extends Component {
  val dataIn = in Vec(UInt(4 bits), 4)
  val dataOut0 = out Bits(16 bits)
  val dataOut1 = out Vec(UInt(4 bits), 4)

  dataOut0 := dataIn.asBits
  dataOut1 := Vec(dataIn.asBits.subdivideIn(4 slices).map(_.asUInt))
}

object OrderExample {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(OrderExample()).doSim { dut =>
      dut.dataIn.zipWithIndex.foreach { case (int, i) => int #= i }
      sleep(2)
    }
  }
}

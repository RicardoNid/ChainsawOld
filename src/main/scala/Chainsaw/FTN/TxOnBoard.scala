package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

case class TxOnBoard() extends Component {

  val txDataGen = TxDataGen()
  val tx = TxWhole(channelInfo)

  val dataOut = master Stream Bits(768 bits)

  txDataGen.dataOut >> tx.dataIn

  dataOut.valid := tx.dataOut.valid
  dataOut.payload := Mux(tx.dataOut.valid, tx.dataOut.payload.asBits, Bits(768 bits).getZero)

}

object TxOnBoard extends App {
  GenRTL(TxOnBoard(), name = "TxOnBoard")
  SimConfig.withFstWave.compile(TxOnBoard()).doSim{dut =>

    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling(500)
  }

}

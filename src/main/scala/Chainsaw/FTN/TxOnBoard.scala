package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

case class TxOnBoard(implicit ftnParams: FtnParams) extends Component {

  val txDataGen = TxDataGen()
  val tx = TxWhole()
  val s2p = DSP.S2P(128,512,UInt(6 bits))
  val txPacking = TxPacking()

  val dataOut = master Stream Bits(768 bits)

  txDataGen.dataOut >> tx.dataIn
  tx.dataOut >> s2p.dataIn
  s2p.dataOut >> txPacking.dataIn
  txPacking.dataOut.payloadMap(_.asBits) >> dataOut
}

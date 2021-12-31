package Chainsaw.FTN

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class Convenc512FTN()
  extends Component with DSPTestable[Bits, Bits] {

  val dataIn = slave Stream Bits(512 bits)
  val dataOut = master Stream Bits(1024 bits)

  val dataIns = dataIn.payload.subdivideIn(4 slices)
  val subConvencs = Seq.fill(4)(Convenc128FTN())

  // top -> sub
  dataIns.zip(subConvencs).foreach{ case (data, conv) =>
    conv.dataIn.payload := data
    conv.dataIn.valid := dataIn.valid
    conv.dataOut.ready := dataOut.ready
  }

  dataOut.payload := subConvencs.map(_.dataOut.payload).asBits()
  dataIn.ready := subConvencs.head.dataIn.ready
  dataOut.valid := subConvencs.head.dataOut.valid

  val latency = 1
}

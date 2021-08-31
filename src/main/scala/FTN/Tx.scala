package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

case class Tx(pF: Int) extends Component {
  val dataIn = slave Flow Fragment(Bits(pF bits))
  //  val dataOut = master Flow Fragment(Vec(fixedType, pF))

  val convencFTN = ConvencFTN(convencConfig, pF)
  val interleaverFTN = InterleaverFTN(interleaveRow, interleaveCol, pF * convencConfig.m)

  val bitAlloc = Array.fill(channelCount / 2)(4)
  val powAlloc = Array.fill(channelCount / 2)(1.0)
  val qammodFTN = QammodFTN(bitAlloc, powAlloc, period = channelCount / pF)
  val IfftFTN = FftFTN(iter = false, inverse = true)

  val dataOut = out(cloneOf(IfftFTN.dataOut))

  dataIn >> convencFTN.dataIn
  convencFTN.dataOut >> interleaverFTN.dataIn
  interleaverFTN.dataOut >> qammodFTN.dataIn
  qammodFTN.dataOut >> IfftFTN.dataIn
  IfftFTN.dataOut >> dataOut

  def latency = convencFTN.latency + interleaverFTN.core.latency + qammodFTN.latency + IfftFTN.latency
}

object Tx {
  def main(args: Array[String]): Unit = {
    GenRTL(Tx(64))
    VivadoSynth(Tx(64))
  }
}



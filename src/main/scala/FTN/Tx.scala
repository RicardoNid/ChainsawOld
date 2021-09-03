package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

case class Tx() extends Component {

  val pF = pFNonIter
  val dataIn = slave Flow Fragment(Bits(pF bits))

  val convencFTN = ConvencFTN(convencConfig, pF)
  val interleaverFTN = InterleaverFTN(params.InterleaveRow, params.InterleaveCol, pF * convencConfig.m)

  val bitAlloc = Array.fill(params.FFTSize / 2)(4)
  val powAlloc = Array.fill(params.FFTSize / 2)(1.0)
  val qammodFTN = QammodFTN(bitAlloc, powAlloc, period = params.FFTSize / pF)
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
    GenRTL(Tx())
    VivadoSynth(Tx())
  }
}



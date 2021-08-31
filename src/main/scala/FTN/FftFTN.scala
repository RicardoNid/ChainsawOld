package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

case class FftFTN(iter: Boolean, inverse: Boolean) extends Component {

  val pF = if (iter) pFIter / 2 else pFNonIter / 2
  val dataIn = slave Flow Fragment(Vec(complexType, pF))
  val dataOut = master Flow Fragment(Vec(complexType, pF))

  var latency = 0

  def getFactors(pF: Int, factors: Seq[Int] = Seq(4)): Seq[Int] = {
    val remained = pF / factors.product
    if (remained < 4) factors :+ remained else getFactors(pF, factors :+ 4)
  }

  if (iter) {

    val core = DSP.FFT.CooleyTukeyFFTStream(pF, dataWidth = 12, coeffWidth = 8, getFactors(pF), inverse)

    latency = core.latency

    core.dataIn.payload := dataIn.fragment
    core.dataIn.valid := dataIn.valid

    dataOut.last := Delay(dataIn.last, core.latency, init = False)

    core.dataOut.ready := True
    dataOut.valid := core.dataOut.valid
    dataOut.fragment := core.dataOut.payload
  } else {

    val core = DSP.FFT.CooleyTukeyBackToBack(N = channelCount / 2, pF,
      factors1 = getFactors(pF), factors2 = getFactors(channelCount / 2 / pF),
      dataWidth = 12, coeffWidth = 8)

    latency = core.latency

    core.dataIn.payload := dataIn.fragment
    core.dataIn.valid := dataIn.valid

    dataOut.last := Delay(dataIn.last, core.latency, init = False)

    core.dataOut.ready := True
    dataOut.valid := core.dataOut.valid
    dataOut.fragment := core.dataOut.payload
  }
}

object FftFTN extends App {
  GenRTL(FftFTN(iter = false, inverse = true))
}

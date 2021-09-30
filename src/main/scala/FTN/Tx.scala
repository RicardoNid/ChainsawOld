package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._

case class Tx() extends Component {

  val pF = pFNonIter
  val dataIn = slave Flow Fragment(Bits(pF bits))



  case class ConvLayerParam(w:Int, i:Int, h:Int)

  val convencFTN = ConvencFTN(convencConfig, pF)
  val interleaverFTN = InterleaverFTN(params.InterleaveRow, params.InterleaveCol, pF * convencConfig.m)
  val qammodFTN = QammodFTN(iter = false)
  val IfftFTN = FftFTN(iter = false, inverse = true)

  val dataOut = out(cloneOf(IfftFTN.dataOut))

  dataIn >> convencFTN.dataIn
  convencFTN.dataOut >> interleaverFTN.dataIn
  interleaverFTN.dataOut >> qammodFTN.dataIn
  //  qammodFTN.dataOut >> IfftFTN.dataIn

  IfftFTN.dataIn.fragment := Vec(qammodFTN.dataOut.fragment.map(_.truncated(ifftFixedType)))
  IfftFTN.dataIn.last := qammodFTN.dataOut.last
  IfftFTN.dataIn.valid := qammodFTN.dataOut.valid

  IfftFTN.dataOut >> dataOut

  Seq(dataIn, convencFTN.dataOut, interleaverFTN.dataOut, qammodFTN.dataOut, IfftFTN.dataOut).foreach(_.addAttribute("mark_debug"))

  def latency = convencFTN.latency + interleaverFTN.core.latency + qammodFTN.latency + IfftFTN.latency
}

object Tx {
  def main(args: Array[String]): Unit = {
    //    GenRTL(Tx())
    val pF = pFNonIter
//    VivadoSynth(ConvencFTN(convencConfig, pF), name = "Convenc400MHz")
    VivadoSynth(InterleaverFTN(params.InterleaveRow, params.InterleaveCol, pF * convencConfig.m), name = "Inter400MHz")
    VivadoSynth(QammodFTN(iter = false), name = "Qam400MHz")
//    VivadoSynth(FftFTN(iter = false, inverse = true), name = "IFFT400MHz")
//    VivadoSynth(DSP.FFT.R2HSIFFT(512, ifftFixedType, coeffType), name = "R2HSIFFT400MHz")

    VivadoSynth(Tx(), name = "TX400M")
  }
}



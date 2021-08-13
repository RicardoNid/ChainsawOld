package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import spinal.lib.bus.bram.BRAM

import Chainsaw._
import Chainsaw.Real

case class DFT(N: Int, dataWidth: Int, coeffWidth: Int) extends Component with CustomPrecision {

  val dataIn = in Vec(dataType(), 2 * N) // complex number stored in bits
  val dataOut = out Vec(dataType(), 2 * N) // complex number stored in bits
  val dataInComplex = (0 until N).indices.map(i => ComplexNumber(dataIn(2 * i), dataIn(2 * i + 1)))
  
//  N match {
//    case 2 =>
//    case 4 => {
//      val A = RegNext(dataInComplex(0) + dataInComplex(2))
//      val B = RegNext(dataInComplex(1) + dataInComplex(3))
//      val C = RegNext(dataInComplex(0) - dataInComplex(2))
//      val D = RegNext(dataInComplex(1) - dataInComplex(3))
//
//      dataOut := Vec(Seq(A + B, C - D.multiplyI, A - B, C + D.multiplyI).map(RegNext(_)))
//    }
//  }

}

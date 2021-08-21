package Chainsaw.DSP.FFT

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.math.sqrt

case class DFT(N: Int, dataWidth: Int, coeffWidth: Int, inverse: Boolean = false) extends Component with CustomPrecision {

  val dataIn = in Vec(complexDataType(), N)
  val dataOut = out Vec(complexDataType(), N)

  def delayed[T <: Data](signal: T, pipelined: Boolean) = if (pipelined) RegNext(signal) else signal

  def butterfly(A: ComplexNumber, B: ComplexNumber) = Seq(A + B, A - B)

  val ret = N match {
    // for 2, 4, 8, use the given box
    case 2 => {
      if (!inverse) Vec(Seq(dataIn(0) + dataIn(1), dataIn(0) - dataIn(1)).map(RegNext(_)))
      else Vec(Seq(dataIn(0) + dataIn(1), dataIn(0) - dataIn(1)).map(complex => RegNext(complex >> 1)))
    }
    case 4 => {
      val A = RegNext(dataIn(0) + dataIn(2))
      val B = RegNext(dataIn(1) + dataIn(3))
      val C = RegNext(dataIn(0) - dataIn(2))
      val D = RegNext(dataIn(1) - dataIn(3))
      if (!inverse) Vec(Seq(A + B, C - D.multiplyI, A - B, C + D.multiplyI).map(RegNext(_)))
      else Vec(Seq(A + B, C + D.multiplyI, A - B, C - D.multiplyI).map(complex => RegNext(complex >> 2)))
    }
    case 8 => {
      if(!inverse){
        // stage 0
        val zipped = dataIn.take(4).zip(dataIn.takeRight(4))
        val A = RegNext(Vec(zipped.map { case (number, number1) => number + number1 } ++ zipped.map { case (number, number1) => number - number1 }))
        // stage 1
        val B = Vec(complexDataType(), 8)
        B(0) := A(0) + A(2)
        B(1) := A(1) + A(3)
        B(2) := A(0) - A(2)
        B(3) := A(1) - A(3)
        B(4) := A(4)
        B(5) := A(5) + A(7)
        B(6) := A(6)
        B(7) := A(5) - A(7)
        val BDelayed = Delay(B, 2)
        // stage 2
        val C = Vec(complexDataType(), 8)
        val CTemp = Vec(complexDataType(), 2) // for dataWidth

        CTemp(0).real := (RegNext(B(5)) * toCoeff(1 / sqrt(2))).real.truncated
        CTemp(0).imag := (RegNext(B(5)) * toCoeff(1 / sqrt(2))).imag.truncated
        CTemp(1).real := (RegNext(B(7)) * toCoeff(1 / sqrt(2))).real.truncated
        CTemp(1).imag := (RegNext(B(7)) * toCoeff(1 / sqrt(2))).imag.truncated

        val CTempDelayed = RegNext(CTemp)

        C(0) := BDelayed(0) + BDelayed(1)
        C(1) := BDelayed(0) - BDelayed(1)
        C(2) := BDelayed(2) - BDelayed(3).multiplyI
        C(3) := BDelayed(2) + BDelayed(3).multiplyI
        C(4) := BDelayed(4) - CTempDelayed(0).multiplyI
        C(5) := BDelayed(4) + CTempDelayed(0).multiplyI
        C(6) := -BDelayed(6).multiplyI + CTempDelayed(1)
        C(7) := BDelayed(6).multiplyI + CTempDelayed(1)
        val CDelayed = RegNext(C)
        // stage 3
        val D0 = CDelayed(0)
        val D1 = CDelayed(4) + CDelayed(6)
        val D2 = CDelayed(2)
        val D3 = CDelayed(4) - CDelayed(6)
        val D4 = CDelayed(1)
        val D5 = CDelayed(5) - CDelayed(7)
        val D6 = CDelayed(3)
        val D7 = CDelayed(5) + CDelayed(7)
        RegNext(Vec(Seq(D0, D1, D2, D3, D4, D5, D6, D7)))
      } else {
        throw new IllegalArgumentException("not implemented parameters")
      }
    }
    // for 3, 5, 7, 9, use Rader-Winograd DFT
  }
  dataOut.zip(ret).foreach { case (out, ret) => out := ret.truncated(HardType(out.real)) }
  def latency = LatencyAnalysis(dataIn(0).real.raw, dataOut(0).real.raw).intValue()
}
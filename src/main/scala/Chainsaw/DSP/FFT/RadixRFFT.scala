package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib._

case class RadixRFFT(N: Int) extends Component {

  val radix = 4
  def isPowR(input: Int): Boolean = if (input == radix) true else isPowR(input / radix)

  val dataWidth = 16
  val coeffWidth = 16

  val peak = log2Up(N) / 2
  val resolution = -(dataWidth - 1 - peak)
  def dataType() = SFix(peak exp, resolution exp)
  def coeffType() = SFix(1 exp, -(coeffWidth - 2) exp)

  val dataIn = in Vec(dataType(), 2 * N) // complex number stored in bits
  val dataOut = out Vec(dataType(), 2 * N) // complex number stored in bits
  val dataInComplex = (0 until N).indices.map(i => ComplexNumber(dataIn(2 * i), dataIn(2 * i + 1)))

  def toSFix: BigDecimal => SFix = SF(_, 1 exp, -(coeffWidth - 2) exp)

  def DFT4(input: Seq[ComplexNumber]): Seq[ComplexNumber] = {
    val A = RegNext(input(0) + input(2))
    val B = RegNext(input(1) + input(3))
    val C = RegNext(input(0) - input(2))
    val D = RegNext(input(1) - input(3))
    Seq(A + B, C - D.multiplyI, A - B, C + D.multiplyI).map(RegNext(_))
  }

  def radix4Coeffs: Int => Seq[ComplexNumber] =
    radixRCoeff(_, 4, N).map(coeff =>
      ComplexNumber(toSFix(coeff.real), toSFix(coeff.imag)))

  def parallelLine(dataIn: Seq[ComplexNumber]) = {
    val size = dataIn.size
    val coeffs = radixRCoeff(size, 4, N)
    def toComplex(coeff: MComplex) = ComplexNumber(toSFix(coeff.real), toSFix(coeff.imag))

    dataIn.zip(coeffs).map { case (data, coeff) =>
      implicit val pipelined = true
      val ret = {
        if (coeff.real == 1.0 && coeff.imag == 0) {
          if (pipelined) Delay(data, 2) else data
        }
        else {
          val retReal, retImag = dataType()
          val full = data * toComplex(coeff)
          retReal := full.real.truncated
          retImag := full.imag.truncated
          ComplexNumber(retReal, retImag)
        }
      }
      //      RegNext(ret)
      ret
    }
  }

  // reorder, no cost on device
  val rawOutput = radixRBuilder(dataInComplex, 4, DFT4, parallelLine)
  val reorderedOutput = doDigitReverse(rawOutput, 4)
  (0 until N).foreach { i =>
    dataOut(2 * i) := reorderedOutput(i).real
    dataOut(2 * i + 1) := reorderedOutput(i).imag
  }
  println(s"expected latency = ${log2Up(N) / 2 * 2 + (log2Up(N) / 2 - 1) * 2}")
  println(s"latency = ${LatencyAnalysis(dataIn(0).raw, dataOut(0).raw)}")
}

object RadixRFFT extends App {
  //  VivadoSynth(new RadixRFFT(64))
  VivadoSynth(new RadixRFFT(16))
}

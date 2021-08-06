package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib.Delay

case class RadixRFFT(N: Int, wordWidth: Int) extends Component {
  val peak = log2Up(N)
  val resolution = -(wordWidth - 1 - peak)
  def fixType() = SFix(peak exp, resolution exp)
  val dataIn = in Vec(fixType(), 2 * N) // complex number stored in bits
  val dataOut = out Vec(fixType(), 2 * N) // complex number stored in bits
  val dataInComplex = (0 until N).indices.map(i => ComplexNumber(dataIn(2 * i), dataIn(2 * i + 1)))

  def toSFix: BigDecimal => SFix =
    SF(_, peak exp, resolution exp)

  def DFT4(input: Seq[ComplexNumber]): Seq[ComplexNumber] = {
    //    Seq(
    //      input.reduce(_ + _),
    //      input(0) - input(2) + (input(3) - input(1)).multiplyI,
    //      input(0) + input(2) - input(1) - input(3),
    //      input(0) - input(2) + (input(1) - input(3)).multiplyI
    //    ).map(RegNext(_))

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
          val retReal, retImag = fixType()
          retReal := (data * toComplex(coeff)).real.truncated
          retImag := (data * toComplex(coeff)).imag.truncated
          ComplexNumber(retReal, retImag)
        }
      }
      RegNext(ret)
    }
  }

  // reorder, no cost on device
  val rawOutput = radixRBuilder(dataInComplex, 4, DFT4, parallelLine)
  val reorderedOutput = doDigitReverse(rawOutput, 4)
  (0 until N).foreach { i =>
    dataOut(2 * i) := reorderedOutput(i).real
    dataOut(2 * i + 1) := reorderedOutput(i).imag
  }
}

object RadixRFFT extends App {
  //  GenRTL(new RadixRFFT(64, 16))
  VivadoSynth(new RadixRFFT(64, 16))
}

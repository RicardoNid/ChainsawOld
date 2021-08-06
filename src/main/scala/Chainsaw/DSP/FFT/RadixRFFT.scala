package Chainsaw.DSP.FFT

import Chainsaw._
import spinal.core._

case class RadixRFFT(N: Int, wordWidth: Int) extends Component {
  val dataIn = in Vec(Bits(wordWidth * 2 bits), N) // complex number stored in bits
  val dataInComplex = dataIn.map(bits =>
    ComplexNumber(bits.splitAt(wordWidth)._1.asSInt.toSFix,
      bits.splitAt(wordWidth)._2.asSInt.toSFix))

  def toSFix: BigDecimal => SFix =
    SF(_, 1 exp, -(wordWidth - 2) exp)

  def DFT4(input: Seq[ComplexNumber]): Seq[ComplexNumber] = Seq(
    input.reduce(_ + _),
    input(0) - input(2) + (input(3) - input(1)).multiplyI,
    input(0) + input(2) - input(1) - input(3),
    input(0) - input(2) + (input(1) - input(3)).multiplyI
  )

  def radix4Coeffs: Int => Seq[ComplexNumber] =
    radixRCoeff(_, 4, N).map(coeff =>
      ComplexNumber(toSFix(coeff.real), toSFix(coeff.imag))
    )

  def parallelLine(dataIn: Seq[ComplexNumber]) = {
    val size = dataIn.size
    dataIn.zip(radix4Coeffs(size)).map { case (data, coeff) => data * coeff }
  }

  // reorder, no cost on device
  val rawOutput = radixRBuilder(dataInComplex, 4, DFT4, parallelLine)
  val reorderedOutput = (0 until N).map(i => rawOutput(digitReverse(i, 4, log2Up(N))))
  val dataOut = out(Vec(radixRBuilder(dataInComplex, 4, DFT4, parallelLine)))
}

object RadixRFFT extends App {
//  GenRTL(new RadixRFFT(64, 8))
  VivadoSynth(new RadixRFFT(64, 8))
}

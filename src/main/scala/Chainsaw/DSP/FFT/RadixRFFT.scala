package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib._

case class RadixRFFT(N: Int = 256) extends Component {

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

  def toCoeff: BigDecimal => SFix = SF(_, 1 exp, -(coeffWidth - 2) exp)

  def DFT4(input: Seq[ComplexNumber]): Seq[ComplexNumber] = {
    val A = RegNext(input(0) + input(2))
    val B = RegNext(input(1) + input(3))
    val C = RegNext(input(0) - input(2))
    val D = RegNext(input(1) - input(3))

    Seq(A + B, C - D.multiplyI, A - B, C + D.multiplyI).map(RegNext(_))
  }

  def parallelLine(dataIn: Seq[ComplexNumber]) = {

    def toComplex(coeff: MComplex) = ComplexNumber(toCoeff(coeff.real), toCoeff(coeff.imag))

    val size = dataIn.size
    val coeffIndices = radixRCoeffIndices(size, 4, N)

    //    println(coeffs.grouped(4).map(_.mkString(" ")).mkString("\n"))

    def multiplyWNnk(signal: ComplexNumber, index: Int): ComplexNumber = {
      implicit val pipelined = true

      // multiply (1 - j) / sqrt(2)
      def multiply1minusj(signal: ComplexNumber) = {
        val retReal, retImag = dataType()
        val A = signal.real + signal.imag
        val B = signal.imag - signal.real
        val A1 = if (pipelined) RegNext(A) else A
        val B1 = if (pipelined) RegNext(B) else B
        val fullReal = A1 * toCoeff(1 / scala.math.sqrt(2.0))
        val fullImag = B1 * toCoeff(1 / scala.math.sqrt(2.0))
        val fullReal1 = if (pipelined) RegNext(fullReal) else fullReal
        val fullImag1 = if (pipelined) RegNext(fullImag) else fullImag
        retReal := fullReal1.truncated
        retImag := fullImag1.truncated
        ComplexNumber(retReal, retImag)
      }

      def delayed(signal: ComplexNumber) = if (pipelined) Delay(signal, 2) else signal
      // deal with trivial values
      val trivialValue = if (N % 8 == 0 && index % (N / 8) == 0) index / (N / 8) else -1
      //      printlnGreen(trivialValue)
      trivialValue match {
        case 0 => delayed(signal)
        case 2 => delayed(-signal.multiplyI)
        case 4 => delayed(-signal)
        case 6 => delayed(signal.multiplyI)

        case 1 => multiply1minusj(signal)
        case 3 => multiply1minusj(-signal.multiplyI)
        case 5 => multiply1minusj(-signal)
        case 7 => multiply1minusj(signal.multiplyI)

        case _ => {
          val retReal, retImag = dataType()
          val full = signal * toComplex(WNnk(N, index))
          retReal := full.real.truncated
          retImag := full.imag.truncated
          ComplexNumber(retReal, retImag)
        }
      }
    }

    case class MultiplyWNnk(index: Int) extends Component {
      setDefinitionName(s"MultiplyWNnk_$index")
      val input = in(Vec(dataType(), 2))
      val output = out(multiplyWNnk(ComplexNumber(input(0), input(1)), index))
    }

    printlnGreen(coeffIndices.mkString(" "))

    dataIn.zip(coeffIndices).map { case (data, index) =>

      //      multiplyWNnk(data, index)
      val multiplier = MultiplyWNnk(index)
      multiplier.input := Vec(data.real, data.imag)
      multiplier.output

      //      implicit val pipelined = true
      //      val ret = {
      //        if (coeff.real == 1.0 && coeff.imag == 0) if (pipelined) Delay(data, 2) else data
      //        else if (coeff.real == -1.0 && coeff.imag == 0) if (pipelined) Delay(-data, 2) else -data
      //        else if (coeff.real == 0.0 && coeff.imag == 1.0) if (pipelined) Delay(data.multiplyI, 2) else data.multiplyI
      //        else {
      //          val retReal, retImag = dataType()
      //          val full = data * toComplex(coeff)
      //          retReal := full.real.truncated
      //          retImag := full.imag.truncated
      //          ComplexNumber(retReal, retImag)
      //        }
      //      }
      //      ret
    }
  }

  // reorder, no cost on device
  val rawOutput = radixRBuilder(dataInComplex, 4, DFT4, parallelLine)
  val reorderedOutput = doDigitReverse(rawOutput, 4)
  (0 until N).foreach { i =>
    dataOut(2 * i) := reorderedOutput(i).real
    dataOut(2 * i + 1) := reorderedOutput(i).imag
  }

  def expectedMult = {

  }

  //  println(s"expected DSP = ${(log2Up(N).toDouble - 2) / 2 * N * (9.toDouble / 16) * 3}")
  println(s"expected latency = ${log2Up(N) / 2 * 2 + (log2Up(N) / 2 - 1) * 2}")
  println(s"latency = ${LatencyAnalysis(dataIn(0).raw, dataOut(0).raw)}")
}

object RadixRFFT extends App {
  //  GenRTL(new RadixRFFT())
  val report = VivadoSynth(new RadixRFFT(), name = "radixRFFT")
}

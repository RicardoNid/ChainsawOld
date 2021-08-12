package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib._

case class CooleyTukeyFFT(N: Int = 256, dataWidth: Int, coeffWidth: Int, factors: Seq[Int]) extends Component {

  val peak = log2Up(N) / 2 // TODO: find a better strategy
  val resolution = -(dataWidth - 1 - peak)
  def dataType() = SFix(peak exp, resolution exp)
  def coeffType() = SFix(1 exp, -(coeffWidth - 2) exp)

  val dataIn = in Vec(dataType(), 2 * N) // complex number stored in bits
  val dataOut = out Vec(dataType(), 2 * N) // complex number stored in bits
  val dataInComplex = (0 until N).indices.map(i => ComplexNumber(dataIn(2 * i), dataIn(2 * i + 1)))

  def toCoeff: BigDecimal => SFix = SF(_, 1 exp, -(coeffWidth - 2) exp)

  def DFT4(input: Seq[ComplexNumber]): Seq[ComplexNumber] = { // TODO: Implement DFTN, N is power of prime number
    val A = RegNext(input(0) + input(2))
    val B = RegNext(input(1) + input(3))
    val C = RegNext(input(0) - input(2))
    val D = RegNext(input(1) - input(3))

    Seq(A + B, C - D.multiplyI, A - B, C + D.multiplyI).map(RegNext(_))
  }

  val mult: (ComplexNumber, Int, Int) => ComplexNumber = multiplyWNnk(_, _, _, dataType, coeffType, Seq(true, true))
  val output = Algos.cooleyTukeyBuilder(dataInComplex, factors, DFT4, mult)
  (0 until N).foreach { i =>
    dataOut(2 * i) := output(i).real
    dataOut(2 * i + 1) := output(i).imag
  }


  //  println(s"expected DSP = ${(log2Up(N).toDouble - 2) / 2 * N * (9.toDouble / 16) * 3}")
  println(s"expected latency = ${log2Up(N) / 2 * 2 + (log2Up(N) / 2 - 1) * 2}")
  println(s"latency = ${LatencyAnalysis(dataIn(0).raw, dataOut(0).raw)}")
}

object CooleyTukeyFFT extends App {
  GenRTL(new CooleyTukeyFFT(16, 16, 16, Seq(4, 4)), name = "FFT")
  val report = VivadoSynth(new CooleyTukeyFFT(N = 256,
    dataWidth = 16, coeffWidth = 16, factors = Seq(4, 4, 4, 4)), name = "radixRFFT")
}

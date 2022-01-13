package Chainsaw

import Chainsaw.algos.{MatlabRefs, Trellis}
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

package object FTN {

  case class ChannelInfo(bitAlloc: Array[Int], powAlloc: Array[Double], bitMask: Array[Int]) {
    val qamPositions = loadFTN1d[Double]("QAMPositions").map(_.toInt).map(_ - 1)
    val qamRemapPositions = loadFTN1d[Double]("QAMRemapPositions").map(_.toInt).map(_ - 1)
  }

  // for qam symbols and twiddle factors
  // for qam symbols and twiddle factors FIXME: use full precision and avoid "Way too big signal"

  val ADDAType = HardType(SInt(6 bits))
  val coeffType = HardType(SFix(1 exp, -15 exp))

  // types of symbols and fft/ifft are closely connected
  val symbolWidth = 16

  def getSymbolType(peak: Int) = HardType(SFix(peak exp, -(symbolWidth - 1 - peak) exp))

  // datapaths and datatypes for fft/ifft
  val fftDecomposition = Seq(4, 4, 4, 4, 2) // the way we construct a 512-point fft
  val ifftShifts = Seq(2, 2, 1, 0, 0)
  val fftShifts = Seq(2, 2, 1, 0, 0)
  val frontFftShifts = Seq(2, 2, 1, 0, 0) // this part has a wider dynamic range

  val Seq(symbolPeak, ifftPeak, fftPeak, addaPeak) =
    Seq(2, 2 + ifftShifts.sum, 2 + ifftShifts.sum + fftShifts.sum, 5)
  val Seq(symbolType, ifftType, fftType, addaFftType) =
    Seq(symbolPeak, ifftPeak, fftPeak, addaPeak).map(getSymbolType)
  val Seq(symbolComplexType, ifftComplexType, fftComplexType, addaFftComplexType) =
    Seq(symbolType, ifftType, fftType, addaFftType).map(toComplexType)

  // for equalizer computation
  val equalWidth = 18

  def getEqualType(peak: Int) = HardType(SFix(peak exp, -(equalWidth - 1 - peak) exp))

  val equalizerWidth = 256
  val smootherType = getEqualType(6) // tap = 16, 2 -> 6 for accumulation
  val smootherComplexType = toComplexType(smootherType)
  val smootherVecType = HardType(Vec(smootherType(), equalizerWidth))
  val smootherComplexVecType = HardType(Vec(ComplexNumber(smootherType), equalizerWidth))

  val equalizationType = getEqualType(6)
  val equalizationComplexType = toComplexType(equalizationType)
  val equalizationVecType = HardType(Vec(equalizationType(), equalizerWidth))
  val equalizationComplexVecType = HardType(Vec(ComplexNumber(equalizationType), equalizerWidth))

  //
  def complexZero = symbolComplexType().getZero

  def rxComplexZero = fftComplexType().getZero

  def loadFTN1d[T](name: String) = eng.load[Array[T]](s"~/FTN326/$name.mat", name)

  def loadFTN2d[T](name: String)(implicit tag: ClassTag[T]) =
    eng.load[Array[Array[T]]](s"~/FTN326/$name.mat", name).transpose.flatten

  val channelInfo = {
    val bitAlloc = loadFTN1d[Double]("bitAlloc").map(_.toInt)
    val powAlloc = loadFTN1d[Double]("powAlloc").map(pow => pow * pow)
    val bitMask = loadFTN1d[Double]("bitMask").map(_.toInt)

    ChannelInfo(bitAlloc, powAlloc, bitMask)
  }

  // loading data for test
  val preambleSymbols = loadFTN1d[Double]("PreambleMappedSymbols").map(_.toInt)

  val matlabTrellis = MatlabRefs.poly2trellis(7, Array(171, 133))
  val trellis = Trellis.fromMatlab(matlabTrellis)

  // common transformations
  def bools2bits(in: Vec[Bool]) = in.reverse.asBits()

  def bits2bools(in: Bits) = Vec(in.asBools.reverse)

  def truncComplex(in: Vec[ComplexNumber], dataType: HardType[SFix]) = Vec(in.map(_.truncated(dataType)))

  def shiftRight(in: Vec[ComplexNumber], shift: Int) = Vec(in.map(_ >> shift))

  // construct hermitian symmetric ifft input
  def ifftPre(in: Vec[ComplexNumber]) = Vec((0 until 512).map {
    case 0 => in.head.getZero
    case 256 => in.head.getZero
    case i => if (i < 256) in(i) else in(512 - i).conj
  })

  // unused channel are masked by zero
  def ifftPost(in: Vec[SFix]) =
    Vec(in.zipWithIndex.map { case (fix, i) => if (i < (channelInfo.bitMask.sum + 1) * 2) fix else fix.getZero })

  // only take half of the real-valued fft results
  def fftPost(in: Vec[ComplexNumber]) = Vec(in.take(in.length / 2))

  def doBitMask(in: Vec[ComplexNumber]) =
    Vec(in.zip(channelInfo.bitMask).map { case (data, mask) => if (mask == 1) data else in.head.getZero })


  // parameter for overall state machine
  val loopLength = 608
  val frameLength = 16
  val iterationNum = 5
}

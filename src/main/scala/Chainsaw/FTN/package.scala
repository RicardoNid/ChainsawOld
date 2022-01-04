package Chainsaw

import Chainsaw.algos.{MatlabRefs, Trellis}
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

package object FTN {

  case class ChannelInfo(bitAlloc: Array[Int], powAlloc: Array[Double], bitMask: Array[Int]){
    val qamPositions = loadFTN1d[Double]("QAMPositions").map(_.toInt).map(_ - 1)
    val qamRemapPositions = loadFTN1d[Double]("QAMRemapPositions").map(_.toInt).map(_ - 1)
  }

  // for qam symbols and twiddle factors
  val unitType = HardType(SFix(2 exp, -13 exp))
  val unitComplexType = toComplexType(unitType)
  // for ifft calculation
  val ifftType = HardType(SFix(8 exp, -7 exp))
  val ifftComplexType = toComplexType(ifftType)
  // for qam symbols and twiddle factors FIXME: use full precision and avoid "Way too big signal"
  val rxUnitType = HardType(SFix(2 exp, -13 exp))
  val rxUnitComplexType = toComplexType(unitType)
  // for fft calculation
  val fftType = HardType(SFix(12 exp, -3 exp))
  val fftComplexType = toComplexType(fftType)
  // for equalizer computation
  val equalizerType = HardType(SFix(6 exp, - 11 exp))
  val equalizerComplexType = toComplexType(equalizerType)
  val equalizerWidth = 256
  val equalizerVecType = HardType(Vec(equalizerType(), equalizerWidth))
  val equalizerComplexVecType = HardType(Vec(ComplexNumber(equalizerType), equalizerWidth))

  def zero = ifftType().getZero

  def rxZero = fftType().getZero

  def complexZero = ifftComplexType().getZero

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

  // data for RxFront
  val rxModulated = loadFTN1d[Double]("rxModulated").map(_ * 512.0)
  val rxMapped = loadFTN1d[MComplex]("rxMapped").map(_.toBComplex)
  val rxEqualized = loadFTN1d[MComplex]("rxEqualized").map(_.toBComplex)

  val rxModulateGolden: Seq[Seq[Double]] = {
    val (preamble, data) = rxModulated.splitAt(1024)
    preamble.grouped(512).map(_.toSeq).toSeq ++ data.grouped(450).map(_.toSeq.padTo(512, 0.0)).toSeq
  }
  val rxMappedGolden: Seq[Seq[BComplex]] = rxMapped.grouped(256).map(_.toSeq).toSeq
  val rxEqualizedGolden: Seq[Seq[BComplex]] = rxEqualized.grouped(256).map(_.toSeq).toSeq

  //

  val matlabTrellis = MatlabRefs.poly2trellis(7, Array(171, 133))
  val trellis = Trellis.fromMatlab(matlabTrellis)

  // common transformations
  def bools2bits(in: Vec[Bool]) = in.reverse.asBits()

  def bits2bools(in: Bits) = Vec(in.asBools.reverse)

  def ifftPre(in: Vec[ComplexNumber]) = Vec((0 until 512).map {
    case 0 => complexZero
    case 256 => complexZero
    case i => if (i < 256) in(i).truncated(ifftType) else in(512 - i).conj.truncated(ifftType)
  })

  def doBitMask(in: Vec[ComplexNumber]) =
    Vec(in.zip(channelInfo.bitMask).map { case (data, mask) => if (mask == 1) data else in.head.getZero })
}

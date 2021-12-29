package Chainsaw

import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

package object FTN {

  case class ChannelInfo(bitAlloc: Array[Int], powAlloc: Array[Double], bitMask: Array[Int]){
    val qamPositions = loadFTN1d[Double]("QAMPositions").map(_.toInt).map(_ - 1)
    val qamRemapPositions = loadFTN1d[Double]("QAMRemapPositions").map(_.toInt).map(_ - 1)
  }

  // transformations


  def bools2bits(in: Vec[Bool]) = in.reverse.asBits()

  def bits2bools(in: Bits) = Vec(in.asBools.reverse)

  // for qam symbols and twiddle factors
  val unitType = HardType(SFix(2 exp, -15 exp))
  val unitComplexType = toComplexType(unitType)
  // for ifft calculation
  val ifftType = HardType(SFix(8 exp, -15 exp))
  val ifftComplexType = toComplexType(ifftType)
  // for qam symbols and twiddle factors FIXME: use full precision and avoid "Way too big signal"
  val rxUnitType = HardType(SFix(2 exp, -13 exp))
  val rxUnitComplexType = toComplexType(unitType)
  // for fft calculation
  val fftType = HardType(SFix(12 exp, -3 exp))
  val fftComplexType = toComplexType(ifftType)
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

  val modulated = loadFTN2d[Double]("recvModulatedSymbolsAllFrame").map(_ * 512.0)
  val preambleModulated = loadFTN2d[Double]("recvPreambleModulatedSymbols").map(_ * 512.0)
  val deModulated = loadFTN2d[MComplex]("deModulatedSymbolsAllFrame").map(_.toBComplex)
  val equalized = loadFTN2d[MComplex]("equalizedSymbolsAllFrame").map(_.toBComplex)
  val deMapped = loadFTN2d[Double]("deMappedBitsAllFrame").map(_.toInt)
  val deInterleaved = loadFTN2d[Double]("deInterleavedBitsAllFrame").map(_.toInt)
  val deCoded = loadFTN2d[Double]("debitsAllFrame").map(_.toInt)

  val modulateGolden: Seq[Seq[Double]] = modulated.grouped(450).map(_.toSeq.padTo(512, 0.0)).toSeq
  val preambleModulatedGolden: Seq[Seq[Double]] = preambleModulated.grouped(256).map(_.toSeq).toSeq
  val deModulatedGolden: Seq[Seq[BComplex]] = deModulated.grouped(256).map(_.toSeq).toSeq
  val equalizedGolden: Seq[Seq[BComplex]] = equalized.grouped(256).map(_.toSeq).toSeq
  val deMappedGolden: Seq[BigInt] = deMapped.grouped(1024).toSeq.map(bit1024 => BigInt(bit1024.mkString(""), 2))
  val deInterleavedGolden: Seq[BigInt] = deMapped.grouped(1024).toSeq.map(bit1024 => BigInt(bit1024.mkString(""), 2))
}

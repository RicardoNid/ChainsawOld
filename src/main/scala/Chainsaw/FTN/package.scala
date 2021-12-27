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
  implicit class StreamUtil[Ti <: Data](stream: Stream[Ti]) {
    def t[To <: Data](transform: Ti => To) = {
      val ret = transform(stream.payload)
      val retStream = Stream(cloneOf(ret))
      retStream.payload := ret
      retStream.valid := stream.valid
      stream.ready := retStream.ready
      retStream
    }
  }

  def bools2bits(in: Vec[Bool]) = in.reverse.asBits()

  def bits2bools(in: Bits) = Vec(in.asBools.reverse)

  // full precision for DSP slice
  val unitType = HardType(SFix(2 exp, -15 exp))
  val unitComplexType = toComplexType(unitType)
  val dataType = HardType(SFix(8 exp, -15 exp))
  val dataComplexType = toComplexType(dataType)
  // to avoid "Way too big signal"
  val rxUnitType = HardType(SFix(2 exp, -13 exp))
  val rxUnitComplexType = toComplexType(unitType)
  val rxDataType = HardType(SFix(12 exp, -3 exp))
  val rxDataComplexType = toComplexType(dataType)

  def zero = dataType().getZero

  def rxZero = rxDataType().getZero

  def complexZero = dataComplexType().getZero

  def rxComplexZero = rxDataComplexType().getZero

  def loadFTN1d[T](name: String) = eng.load[Array[T]](s"~/FTN326/$name.mat", name)

  def loadFTN2d[T](name: String)(implicit tag: ClassTag[T]) =
    eng.load[Array[Array[T]]](s"~/FTN326/$name.mat", name).transpose.flatten

  val channelInfo = {
    val bitAlloc = loadFTN1d[Double]("bitAlloc").map(_.toInt)
    val powAlloc = loadFTN1d[Double]("powAlloc").map(pow => pow * pow)
    val bitMask = loadFTN1d[Double]("bitMask").map(_.toInt)

    ChannelInfo(bitAlloc, powAlloc, bitMask)
  }
}

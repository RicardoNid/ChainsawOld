package Chainsaw.dsl.transform

import Chainsaw.DSP.FFT.{CooleyTukeyFFT, CooleyTukeyHSIFFT}
import Chainsaw.{dsl, toComplexType}
import Chainsaw.dsl._
import breeze.math._
import spinal.core._
import spinal.lib.Delay
import HSIFFT._
import breeze.linalg.DenseVector

import scala.reflect.ClassTag

class HSIFFT[T: ClassTag]
(val size: Int)
(implicit typeInOut: MixType[Complex])
  extends dsl.BaseTransform[Complex, Complex](getAlgo, new HSIFFTImpl(size))

object HSIFFT {

  def apply[T: ClassTag](size: Int)(implicit typeInOut: MixType[Complex]): HSIFFT[T] = new HSIFFT(size)

  def getAlgo = (dataIn: Array[Complex]) => Chainsaw.algos.Dft.idft(DenseVector(dataIn)).toArray

}

class HSIFFTImpl(theSize: Int) extends Impl {
  override val name = "HSIFFT"
  val st = HardType(SFix(2 exp, -13 exp))
  override val foldMax = theSize
  override val width = (32, 32)
  override val size = (theSize, theSize)
  override def getLatency(fold: Int) = CooleyTukeyFFT(4, true, st, st, Seq(4)).latency
  override def getFunction(fold: Int) = (dataIn: Vec[Bits]) => {
    val core = CooleyTukeyFFT(4, true, st, st, Seq(4))
    core.dataIn.payload.zip(dataIn).foreach { case (port, data) => port.assignFromBits(data) }
    core.dataIn.valid := True
    Vec(core.dataOut.payload.map(_.asBits))
  }
}

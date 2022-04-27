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

  override val foldMax = theSize

  override val size = (theSize, theSize)

  override def getImpl(fold: Int) = {
    val st = HardType(SFix(2 exp, -13 exp))
    val component = CooleyTukeyFFT(theSize, true, st, st, Seq(4))
    val impl = (dataIn: (Vec[Bits], Bool)) => {
      component.dataIn.payload.zip(dataIn._1).foreach { case (port, data) => port.assignFromBits(data) }
      component.dataIn.valid := True
      val ret = Vec(component.dataOut.payload.map(_.asBits))
      (ret, Delay(dataIn._2, component.latency, init = False))
    }
    RawImpl(impl, 0)
  }
}

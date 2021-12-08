package Chainsaw.fastAlgos

import Chainsaw._
import Chainsaw.fastAlgos.Convolution.{cyclicConvolve, linearConvolve}
import Chainsaw.fastAlgos.Qammod.{qammod}
import breeze.linalg.DenseVector
import org.scalatest.flatspec.AnyFlatSpec

class MatlabRefsTest extends AnyFlatSpec {

  val As = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))
  val Bs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(8))
  val Cs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))

  def assertClose(a: DenseVector[BComplex], b: DenseVector[BComplex]) = assert(a ~= b)

  "linear convolution" should "work" in {
    As.zip(Bs).foreach { case (data, kernel) =>
      val breeze = linearConvolve(data, kernel)
      val matlab = MatlabRefs.conv(data, kernel)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  "cyclic convolution" should "work" in {
    As.zip(Cs).foreach { case (data, kernel) =>
      val breeze = cyclicConvolve(data, kernel)
      val matlab = MatlabRefs.cconv(data, kernel)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  import breeze.signal.fourierTr

  "dft" should "work" in {
    As.foreach { data =>
      val breeze = fourierTr.dvComplex1DFFT(data)
      val matlab = MatlabRefs.dft(data)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  val symbols = (0 until 100).map(_ => ChainsawRand.nextInt(256)).asDv

  "qammod" should "work" in {
    (2 to 8).foreach{ i =>
      val modulationOrder = 1 << i
      val bits: DenseVector[Int] = (0 until 100).map(_ => ChainsawRand.nextInt(modulationOrder)).asDv
      val breeze = Qammod.qammod(bits, modulationOrder)
      val matlab = MatlabRefs.qammod(bits, modulationOrder)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }


}


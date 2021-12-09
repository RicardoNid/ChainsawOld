package Chainsaw.fastAlgos

import Chainsaw._
import Chainsaw.fastAlgos.Convolution.{cyclicConvolve, linearConvolve}
import Chainsaw.fastAlgos.Qam.qammod
import Chainsaw.matlabIO.{MComplex, eng}
import breeze.linalg.DenseVector
import org.scalatest.flatspec.AnyFlatSpec

class MatlabRefsTest extends AnyFlatSpec {

  val As = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))
  val Bs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(8))
  val Cs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))


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

  "breeze dft" should "work" in {
    As.foreach { data =>
      val breeze = fourierTr.dvComplex1DFFT(data)
      val matlab = MatlabRefs.dft(data)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  "my dft" should "work" in {
    As.foreach { data =>
      val mine = new DenseVector(Dft.dft(data.toArray))
      val matlab = MatlabRefs.dft(data)
      assert(mine ~= matlab, s"\nmine  : $mine  \nmatlab: $matlab")
    }
  }

  "qammod" should "work" in {
    Seq(2, 3, 4, 6, 8).foreach { i =>
      val modulationOrder = 1 << i
      val bits: DenseVector[Int] = (0 until 100).map(_ => ChainsawRand.nextInt(modulationOrder)).asDv
      val breeze = Qam.qammod(bits, modulationOrder)
      val matlab = MatlabRefs.qammod(bits, modulationOrder)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  "qamdemod" should "work" in {
    Seq(2, 3, 4, 6, 8).foreach { i =>
      val modulationOrder = 1 << i
      val symbols = (0 until 100).map(_ => ChainsawRand.nextComplex(-1, 1)).asDv
      val breeze = Qam.qamdemod(symbols, modulationOrder)
      val matlab = MatlabRefs.qamdemod(symbols, modulationOrder)
      assert(breeze.equals(matlab), s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }



}


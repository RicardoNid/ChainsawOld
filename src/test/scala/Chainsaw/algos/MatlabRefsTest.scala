package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.Convolution.{genericCyclicConvolve, genericLinearConvolve}
import Chainsaw.algos.Qam.qammod
import Chainsaw.matlabIO.{MComplex, eng}
import breeze.linalg.DenseVector
import org.scalatest.flatspec.AnyFlatSpec

class TMatlabRefsTest extends AnyFlatSpec {

  val As = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))
  val Bs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(8))
  val Cs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))


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
      val mine = Dft.dft(data)
      val matlab = MatlabRefs.dft(data)
      assert(mine ~= matlab, s"\nmine  : $mine  \nmatlab: $matlab")
    }
  }

  "qammod" should "work" in {
    Seq(2, 3, 4, 6, 8).foreach { i =>
      val modulationOrder = 1 << i
      val bits: DenseVector[Int] = (0 until 100).map(_ => ChainsawRand.nextInt(modulationOrder)).toDv
      val breeze = Qam.qammod(bits, modulationOrder)
      val matlab = MatlabRefs.qammod(bits, modulationOrder)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  "qamdemod" should "work" in {
    Seq(2, 3, 4, 6, 8).foreach { i =>
      val modulationOrder = 1 << i
      val symbols = (0 until 100).map(_ => ChainsawRand.nextComplex(-1, 1)).toDv
      val breeze = Qam.qamdemod(symbols, modulationOrder)
      val matlab = MatlabRefs.qamdemod(symbols, modulationOrder)
      assert(breeze.equals(matlab), s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }



}


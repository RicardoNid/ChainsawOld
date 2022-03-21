package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.Convolution._
import org.scalatest.flatspec.AnyFlatSpec

class ConvolutionTest extends AnyFlatSpec {

  val size           = 16
  val complexData    = (0 until 100).map(_ => ChainsawRand.nextComplexDV(size))
  val complexKernels = (0 until 100).map(_ => ChainsawRand.nextComplexDV(size))

  behavior of "ConvolutionTest"

  it should "genericLinearConvolve" in {

    complexData.zip(complexKernels).foreach { case (data, kernel) =>
      val breeze = genericLinearConvolve(data, kernel)
      val matlab = MatlabRefs.conv(data, kernel)
      println(s"\nbreeze: $breeze\nmatlab: $matlab")
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

  it should "genericCyclicConvolve" in {
    complexData.zip(complexKernels).foreach { case (data, kernel) =>
      val breeze = genericCyclicConvolve(data, kernel)
      val matlab = MatlabRefs.cconv(data, kernel)
      assert(breeze ~= matlab, s"\nbreeze: $breeze\nmatlab: $matlab")
    }
  }

}

package Chainsaw.DSP.FFT

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.primes._
import Chainsaw.matlabIO._
import Chainsaw._

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

object Algos extends App {

  // build radix-r Cooley-Tukey FFT by the "block" and "parallel line" given
  def cooleyTukeyBuilder[T](input: Seq[T], factors: Seq[Int],
                            block: Seq[T] => Seq[T],
                            mult: (T, Int, Int) => T, inverse: Boolean = false): Seq[T] = {

    def twiddle(input: Seq[Seq[T]]) = {
      val N1 = input.head.size
      val N2 = input.size
      input.zip(cooleyTukeyCoeffIndices(N1, N2))
        .map { case (ts, ints) => ts.zip(ints)
          .map { case (t, i) => mult(t, if (!inverse) i else -i, N1 * N2) }
        }
    }

    def recursiveBuild(input: Seq[T], factors: Seq[Int]): Seq[T] = {

      if (factors.size == 1) block(input)
      else {
        val N1 = factors.head
        val N2 = input.size / N1

        import DSP.interleave.Algos._
        val input2D = matIntrlv1D2D(input, N1, N2) // permutation 0
        //        printlnGreen("after inter0")
        //        println(input2D.map(_.mkString(" ")).mkString("\n"))
        val afterBlock = input2D.map(block) // N2 blocks, length = N1
        //        printlnGreen("after N1 FFT")
        printlnGreen("before twiddle")
        //        println(afterBlock.map(_.mkString(" ")).mkString("\n"))
        val afterParallel = twiddle(afterBlock)
        //        printlnGreen("before inter1")
        //        println(afterParallel.map(_.mkString(" ")).mkString("\n"))
        val input2DForRecursion = matIntrlv2D2D(afterParallel, N2, N1) // permutation 1(transpose)
        val afterRecursion = input2DForRecursion.map(recursiveBuild(_, factors.tail))
        //        println(afterRecursion.map(_.mkString(" ")).mkString("\n"))
        val ret = matIntrlv2D1D(afterRecursion, N1, N2) // permutation 2
        ret
      }
    }
    recursiveBuild(input, factors)
  }

  def cooleyTukeyCoeffIndices(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)
  def cooleyTukeyCoeff(N1: Int, N2: Int) = cooleyTukeyCoeffIndices(N1, N2).map(_.map(WNnk(N1 * N2, _)))

  def cooleyTukeyFFT(input: Seq[MComplex], factors: Seq[Int]) = {
    def block(input: Seq[MComplex]) = Refs.FFT(input.toArray).toSeq
    def mult(input: MComplex, index: Int, N: Int) = input * WNnk(N, index)
    cooleyTukeyBuilder(input, factors, block, mult)
  }

  def radixRFFT(input: Seq[MComplex], radix: Int) = {
    val N = input.size
    def buildFactors(factors: Seq[Int]): Seq[Int] = if (factors.product == N) factors else buildFactors(factors :+ radix)
    cooleyTukeyFFT(input, buildFactors(Seq(radix)))
  }


  def CyclicConv(input: Seq[MComplex], coeff: Seq[MComplex], L: Int): Seq[MComplex] = {
    require(L >= input.size && L >= coeff.size)
    // TODO: expand this for different size relationship
    val paddedInput = input.padTo(L, new MComplex(0, 0))
    val paddedCoeff = coeff.padTo(L, new MComplex(0, 0)).reverse
    (1 to L).map { i =>
      val rotatedCoeff = paddedCoeff.takeRight(i) ++ paddedCoeff.take(L - i) // rotate right
      paddedInput.zip(rotatedCoeff).map { case (complex, complex1) => complex * complex1 }.reduce(_ + _)
    }
  }

  def raderDFT(input: Seq[MComplex]) = {
    val N = input.size
    require(SmallPrimes.isPrime(N))

    def getGenerator(modulus: Int) = {
      def isGenerator(element: Int): Boolean = (1 until modulus).map(i => Zp(modulus).pow(element, i).intValue()).toSet == (1 until modulus).toSet
      (1 until modulus).dropWhile(!isGenerator(_)).head
    }

    val g = getGenerator(N)
    val inputPermutation = (0 until N - 1).map(N - 1 - _).map(i => Zp(N).pow(g, i).intValue())
    val coeffPermutation = (0 until N - 1).map(i => Zp(N).pow(g, i).intValue())
    val outputPermutation = (0 until N - 1).map(i => Zp(N).pow(g, i).intValue())

    val permutatedInput = inputPermutation.map(input(_))
    val permutatedCoeff = coeffPermutation.map(WNnk(N, _) - new MComplex(1, 0))
    val permutatedOutput: Seq[MComplex] = CyclicConv(permutatedInput, permutatedCoeff, N - 1)

    val output: ArrayBuffer[MComplex] = ArrayBuffer.fill(N)(new MComplex(0, 0)) // output1, output2...output(N-1)
    output(0) = input.reduce(_ + _)
    permutatedOutput.zip(outputPermutation).foreach { case (complex, i) => output(i) = complex + output(0) }

    output
  }


}

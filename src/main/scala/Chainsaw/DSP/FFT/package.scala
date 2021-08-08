package Chainsaw.DSP

import Chainsaw._
import matlabIO._
import spinal.core._
import scala.util.{Failure, Success, Try}

package object FFT {
  val eng = AsyncEng.get()
  def FFTRef(input: Array[MComplex]): Array[MComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("fft", input))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("fft", input).map(new MComplex(_, 0))
      case Success(value) => value
    }
  }

  import scala.math.Pi

  def WNnk(N: Int, nk: Int): MComplex = {
    val ret = Try(eng.feval[MComplex]("exp", new MComplex(0, -2 * Pi * nk / N))) // TODO: Pi may not be accurate enough
    ret match {
      case Failure(exception) => new MComplex(eng.feval[Double]("exp", new MComplex(0, -2 * Pi * nk / N)), 0)
      case Success(value) => value
    }
  }
  def WNnk(N: Int, n: Int, k: Int): MComplex = WNnk(N, n * k)

  // in fact, it is a radix-r builder, "butterfly" can be replaced by radix-r DFT
  def radixRBuilder[T](input: Seq[T], radix: Int, block: Seq[T] => Seq[T], parallelLine: Seq[T] => Seq[T]): Seq[T] = {
    val N = input.size
    require(isPow2(N))

    def recursiveBuild(input: Seq[T]): Seq[T] = {
      val size = input.size
      if (size == radix) block(input)
      else {
        val segmentSize = size / radix
        val segments = input.grouped(segmentSize).toSeq
        val rets = (0 until segmentSize).map { i =>
          val blockInput = segments.map(_.apply(i)) // zip all segments together
          block(blockInput) // apply the basic transform
        }
        val mid = Seq.tabulate(radix, segmentSize)((i, j) => rets(j)(i)).flatten // reorder
        val nextInput = parallelLine(mid)
        nextInput.grouped(segmentSize).toSeq.map(recursiveBuild(_)).reduce(_ ++ _)
      }
    }
    recursiveBuild(input)
  }

  def digitReverse(input: Int, radix: Int, bitLength: Int): Int = {
    require(isPow2(radix))
    val digitWidth = log2Up(radix)
    import scala.math.ceil
    val digitStrings = toWordStrings(BigInt(input), digitWidth, ceil(bitLength / digitWidth).toInt)
    BigInt(digitStrings.reverse.mkString(""), 2).toInt
  }

  def doDigitReverse[T](input: Seq[T], radix: Int) = {
    input.indices.map(i => input(digitReverse(i, radix, log2Up(input.size))))
  }

  def swapByButterfly(input: Seq[Int]) = {
    def butterfly(input: Seq[Int]) = Seq(input(1), input(0)) // swap
    def parallelLine(dataIn: Seq[Int]) = dataIn
    radixRBuilder(input, 2, butterfly, parallelLine)
  }

  def elementWise(a: Seq[MComplex], b: Seq[MComplex], operator: (MComplex, MComplex) => MComplex) =
    a.zip(b).map { case (complex, complex1) => operator(complex, complex1) }

  def radixRCoeffIndices(size: Int, radix: Int, N: Int) = {
    val segmentSize = size / radix
    val baseGap = N / size
    Seq.tabulate(radix, segmentSize)(baseGap * _ * _).flatten
  }

  def radixRCoeff(size: Int, radix: Int, N: Int) = radixRCoeffIndices(size, radix, N).map(i => WNnk(N, i))

  def parallelLine(dataIn: Seq[MComplex], radix: Int, N: Int) = {
    val size = dataIn.size
    val coeffs = radixRCoeff(size, radix, N)
    dataIn.zip(coeffs).map { case (data, coeff) => data * coeff }
  }

  def radix2FFT(input: Array[MComplex]): Seq[MComplex] = {
    val N = input.size

    def butterfly(input: Seq[MComplex]): Seq[MComplex] = Seq(input(0) + input(1), input(0) - input(1))
    def parallel2: Seq[MComplex] => Seq[MComplex] = parallelLine(_, 2, N)
    val ret = radixRBuilder(input, 2, butterfly, parallel2)
    (0 until N).map(i => ret(digitReverse(i, 2, log2Up(N))))
  }

  def radix4FFT(input: Array[MComplex]): Seq[MComplex] = {
    val N = input.size
    def DFT4(input: Seq[MComplex]): Seq[MComplex] = Seq(
      input.reduce(_ + _),
      input(0) - input(2) + new MComplex(0, 1) * (input(3) - input(1)),
      input(0) + input(2) - input(1) - input(3),
      input(0) - input(2) + new MComplex(0, 1) * (input(1) - input(3))
    )
    def parallel4: Seq[MComplex] => Seq[MComplex] = parallelLine(_, 4, N)
    val ret = radixRBuilder(input, 4, DFT4, parallel4)
    (0 until N).map(i => ret(digitReverse(i, 4, log2Up(N))))
  }
}

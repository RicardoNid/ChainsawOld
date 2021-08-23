package Chainsaw.DSP

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

import scala.util.{Failure, Success, Try}
import scala.math.Pi

package object FFT {
  val eng = AsyncEng.get()

  def WNnk(N: Int, nk: Int): MComplex = {
    val ret = Try(eng.feval[MComplex]("exp", new MComplex(0, -2 * Pi * nk / N)))
    ret match {
      case Failure(exception) => new MComplex(eng.feval[Double]("exp", new MComplex(0, -2 * Pi * nk / N)), 0)
      case Success(value) => value
    }
  }
  def WNnk(N: Int, n: Int, k: Int): MComplex = WNnk(N, n * k)

  def isPowR(input: Int, radix: Int): Boolean = if (input == radix) true else isPowR(input / radix, radix)

  def multiplyWNnk(signal: ComplexNumber, index: Int, N: Int,
                   dataType: HardType[SFix], coeffType: HardType[SFix],
                   pipeline: Seq[Boolean]): ComplexNumber = {

    def pipelined[T <: Data](signal: T, i: Int) = if (pipeline(i)) RegNext(signal) else signal

    def toFixedCoeff: Double => SFix = SF(_, coeffType().maxExp exp, coeffType().minExp exp)
    // multiply (1 - j) / sqrt(2)
    def multiply1minusj(signal: ComplexNumber) = {
      val retReal, retImag = dataType()
      val A = signal.real + signal.imag
      val B = signal.imag - signal.real
      val fullReal = pipelined(A, 0) * toFixedCoeff(1 / scala.math.sqrt(2.0))
      val fullImag = pipelined(B, 0) * toFixedCoeff(1 / scala.math.sqrt(2.0))
      retReal := pipelined(fullReal, 1).truncated
      retImag := pipelined(fullImag, 1).truncated
      ComplexNumber(retReal, retImag)
    }

    def delayed(signal: ComplexNumber) = Delay(signal, pipeline.filter(_ == true).size)
    // trivial values
    val trivialValue = if (N % 8 == 0 && index % (N / 8) == 0) index / (N / 8) else -1
    trivialValue match {
      case 0 => delayed(signal)
      case 2 => delayed(-signal.multiplyI)
      case 4 => delayed(-signal)
      case 6 => delayed(signal.multiplyI)

      case 1 => multiply1minusj(signal)
      case 3 => multiply1minusj(-signal.multiplyI)
      case 5 => multiply1minusj(-signal)
      case 7 => multiply1minusj(signal.multiplyI)

      case _ => { // nontrivial values
        val retReal, retImag = dataType()
        val coeffValue = WNnk(N, index)
        val fixedCoeff = ComplexNumber(toFixedCoeff(coeffValue.real), toFixedCoeff(coeffValue.imag))
        val full = signal.fastMult(fixedCoeff, false +: pipeline)
        retReal := full.real.truncated
        retImag := full.imag.truncated
        ComplexNumber(retReal, retImag)
      }
    }
  }

  // build radix-r Cooley-Tukey FFT by the "block" and "parallel line" given
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

  def randComplex(length: Int) = {
    val test = (0 until 2 * length).map(_ => (DSPRand.nextDouble() - 0.5) * 16)
    (0 until length).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray
  }
}

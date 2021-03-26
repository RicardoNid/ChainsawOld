package projects

import breeze.numerics.constants.Pi
import breeze.numerics.{cos, floor, sin}
import scala.math.pow

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

package object FTN {


  // typedefs
  val naturalWidth = 8
  val fractionalWidth = 8
  val bitWidth = naturalWidth + fractionalWidth

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  val testFFTLength = 8

  def Double2Fix(value: Double) = floor(value * (1 << fractionalWidth)).toInt // convert Double to valid stimulus for simulation
  def Fix2Double(value: SFix) = value.raw.toBigInt.toDouble / pow(2, fractionalWidth)

  // OPTIMIZE: implement prime & factor by table
  def isPrime(n: Int): Boolean = {
    if (n <= 1)
      false
    else if (n == 2)
      true
    else
      !(2 until n).exists(n % _ == 0)
  }

  def factorize(N: Int): ArrayBuffer[Int] = {
    if (isPrime(N)) ArrayBuffer(N)
    else {
      val factor = (2 until N).find(N % _ == 0).get
      val result = factorize(N / factor)
      result.insert(0, factor)
      result
    }
  }

  def winogradDFT(input: IndexedSeq[ComplexNumber]) = {

    val N = input.length

    require(isPrime(N), s"Winograd DFT is for prime number")
    require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

    val output = Array.ofDim[ComplexNumber](N)
    if (N == 2) {
      val s0 = input(0) + input(1)
      val s1 = input(0) - input(1)
      val m0 = s0
      val m1 = s1
      output(0) = m0
      output(1) = m1
    }

    //    if(N == 3){
    //      val s1 = input(1) + input(2)
    //      val s2 = input(1) - input(2)
    //      val s3 = input(0) - input(1)
    //
    //      val m0 = s3
    //      val m1 = s1 * ComplexNumber(cos(2 * Pi / 3) - 1, 0)
    //      val m2 = s2 * ComplexNumber(cos(2 * Pi / 3) - 1, 0)
    //    }

    output
  }

  def cooleyTukeyFFT(input: IndexedSeq[ComplexNumber]): IndexedSeq[ComplexNumber] = {

    val N = input.length

    val factors = factorize(N)
    val factor1 = factors(0)
    val factor2 = factors.reduce(_ * _) / factor1

    val outputNumbers = Array.ofDim[ComplexNumber](N)

    if (isPrime(N)) (0 until N).foreach(i => outputNumbers(i) = winogradDFT(input)(i))
    else {

      // W_{N}^{k n} = \mathrm{e}^{-\mathrm{j} 2 \pi k n / N}
      def coefficient(n2: Int, k1: Int) = ComplexNumber(cos(-2 * Pi * n2 * k1 / N), sin(-2 * Pi * n2 * k1 / N))

      val coefficients = Array.tabulate(factor2, factor1)((k1, n2) => coefficient(n2, k1)).flatten

      val cooleyGroups = input.zipWithIndex.sortBy(_._2 % factor1).map(_._1).grouped(factor2).toArray

      val stage1Numbers = cooleyGroups.map(cooleyTukeyFFT(_)).flatten.map(_.tap)

      val stage2Numbers = stage1Numbers.zipWithIndex.sortBy(_._2 % factor2).map(_._1).zip(coefficients).map { case (number, coeff) => number * coeff }

      val winoGroups = stage2Numbers.grouped(factor1).toArray

      val winoResults = winoGroups.map(winogradDFT(_)).flatten

      (0 until N).foreach(i => outputNumbers(i % factor1 * factor2 + i / factor1) = winoResults(i))
    }

    outputNumbers
  }

}

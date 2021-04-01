import breeze.numerics.floor
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.math.pow

package object DSP {

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


}

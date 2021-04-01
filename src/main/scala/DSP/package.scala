import breeze.numerics.{floor, pow}
import spinal.core._
import spinal.core.sim._
import scala.collection.mutable.ArrayBuffer

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

  def optimalCSD(num: Int):String = {
    val pattern0 = "1101".r
    val pattern1 = "11+0".r
    val pattern2 = "901".r

    var string = num.toBinaryString.reverse + "0"
    var done0 = false
    var done1 = false

    while(!done0){
      val sub = pattern0.findFirstIn(string).getOrElse(
        pattern1.findFirstIn(string).getOrElse {
          done0 = true
          " "
        }
      )
      if (sub == "1101") string = string.replaceFirst(sub, "9011")
      else string = string.replace(sub, "9" + "0" * (sub.length - 2) + "1")
    }

    while(!done1){
      val sub = pattern2.findFirstIn(string).getOrElse{
        done1 = true
        " "
      }
      string = string.replaceFirst(sub, "110")
    }

    assert(verifyCSD(string, num))
    string.reverse.dropWhile(_ == '0')
  }

  def classicCSD(num: Int):String = {
    val pattern = "11+0".r

    var string = num.toBinaryString.reverse + "0"
    var done = false
    while(!done){
      val sub: Option[String] = pattern.findFirstIn(string)
      sub match {
        case Some(x) => string = string.replaceFirst(x, "9" + "0" * (x.length - 2) + "1")
        case None => done = true
      }
    }
    string.reverse
  }

  def verifyCSD(res:String, num:Int): Boolean ={
    (0 until res.length).map { i =>
      res(i) match {
        case '0' => 0
        case '1' => 1 * pow(2, i)
        case '9' => -1 * pow(2, i)
      }
    }.sum == num
  }

}

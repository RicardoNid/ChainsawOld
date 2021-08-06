package FTN

import Chainsaw._
import FTN.IfftFTN.{WNnk, butterflyBuilder}
import matlabIO._
import spinal.core._

import scala.util.{Failure, Success, Try}

// IFFT for FTN, specially designed for conjugate symmetric input
// TODO: optimize for conjugate symmetric input
class IfftFTN(N: Int) extends Component {

  val dataIn = in Vec(Bits(wordWidth * 2 bits), N)
  val dataInComplex = dataIn.map(bits =>
    ComplexNumber(bits.splitAt(wordWidth)._1.asSInt.toSFix,
      bits.splitAt(wordWidth)._2.asSInt.toSFix))

  def butterfly(pairIn: Seq[ComplexNumber]) = Vec(pairIn(0) + pairIn(1), pairIn(0) - pairIn(1))
  def line(dataIn: Seq[ComplexNumber], N: Int) = {
    val size = dataIn.size
    val half = size / 2
    val gap = N / size
    (0 until size).map { i =>
      val signal = dataIn(i)
      if (i < half) signal
      else {
        val coeff = WNnk(N, (i - half) * gap)
        val coeffSignal = ComplexNumber(toSFix(coeff.real), toSFix(coeff.imag))
        signal * coeffSignal
      }
    }
  }

  val dataOut = out(Vec(butterflyBuilder(dataInComplex, butterfly, line)))
}

object IfftFTN extends App {
  // the algo of butterfly

  import scala.math.Pi

  def WNnk(N: Int, nk: Int): MComplex = {
    println(s"invoke WN($nk)")
    val ret = Try(eng.feval[MComplex]("exp", new MComplex(0, -2 * Pi * nk / N))) // TODO: Pi may not be accurate enough
    ret match {
      case Failure(exception) => new MComplex(eng.feval[Double]("exp", new MComplex(0, -2 * Pi * nk / N)), 0)
      case Success(value) => value
    }
  }

  // TODO: do optimization for DSPs
  def butterflyBuilder[T](input: Seq[T], butterfly: Seq[T] => Seq[T], parallelLine: (Seq[T], Int) => Seq[T]): Seq[T] = {
    val N = input.size
    require(isPow2(N))
    def recursiveBuild(input: Seq[T]): Seq[T] = {
      val size = input.size
      size match {
        case 2 => butterfly(input)
        case _ => {
          val half = size / 2
          val up = input.take(half)
          val down = input.takeRight(half)
          val rets = up.zip(down).map { case (t, t1) => butterfly(Seq(t, t1)) } // do butterfly
          val mid = Seq.tabulate(2, half)((i, j) => rets(j)(i)).flatten // reorder
          println(mid.mkString(" "))
          val nextInput = parallelLine(mid, N)
          recursiveBuild(nextInput.take(half)) ++ recursiveBuild(nextInput.takeRight(half))
        }
      }
    }
    recursiveBuild(input)
  }

  def Algo(input: Array[MComplex]): Seq[MComplex] = {
    def butterfly(dataIn: Seq[MComplex]) = Seq(dataIn(0) + dataIn(1), dataIn(0) - dataIn(1))
    def line(dataIn: Seq[MComplex], N: Int) = {
      val size = dataIn.size
      val half = size / 2
      val gap = N / size
      (0 until size).map(i => if (i < half) dataIn(i) else dataIn(i) * WNnk(N, (i - half) * gap))
    }
    butterflyBuilder(input, butterfly, line)
  }



  GenRTL(new IfftFTN(128))
  VivadoSynth(new IfftFTN(512))
}

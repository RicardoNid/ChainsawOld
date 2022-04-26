package Chainsaw.examples.FFTOTA

import Chainsaw.DSP.FFT.eng
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}
import scala.math.Pi

object Algos {
  def CooleyTukeyBuilder[T](factors: Seq[Int], block: Seq[T] => Seq[T], line: (Seq[T], Int, Int) => Seq[T]): Seq[T] => Seq[T] = {

    def buildRecursively(dataIn: Seq[T], factors: Seq[Int]): Seq[T] = {
      if (factors.size == 1) block(dataIn)
      else {
        val N1 = factors.head
        val N2 = factors.tail.product
        println(s"$N1, $N2")
        val dataForBlockA  = DSP.interleave.Algos.matIntrlv(dataIn, N1, N2)
        val afterBlockA    = dataForBlockA.grouped(N1).map(block(_)).toSeq
        val afterLine      = line(afterBlockA.flatten, N1, N2)
        val dataForBlockB  = DSP.interleave.Algos.matIntrlv(afterLine, N2, N1)
        val afterRecursion = dataForBlockB.grouped(N2).map(buildRecursively(_, factors.tail)).toSeq
        DSP.interleave.Algos.matIntrlv(afterRecursion.flatten, N1, N2)
      }
    }

    (dataIn: Seq[T]) => buildRecursively(dataIn, factors)
  }

  def CooleyTukeyCoeffs(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1).flatten

  def WNnk(N: Int, nk: Int): BComplex = {
    val ret = Try(eng.feval[BComplex]("exp", new BComplex(0, -2 * Pi * nk / N)))
    ret match {
      case Failure(exception) => new BComplex(eng.feval[Double]("exp", new BComplex(0, -2 * Pi * nk / N)), 0)
      case Success(value)     => value
    }
  }

  def DFT(dataIn: Seq[BComplex]) = DSP.FFT.Refs.FFT(dataIn.toArray)

  def main(args: Array[String]): Unit = {
    val mult = (dataIn: Seq[BComplex], N1: Int, N2: Int) =>
      dataIn.zip(CooleyTukeyCoeffs(N1, N2).map(WNnk(N1 * N2, _))).map { case (data, coeff) => data * coeff }
    val ctfft = CooleyTukeyBuilder(Seq(2, 2, 2), DFT, mult)

    val testCase: Seq[BComplex] = (0 until 8).map(_ => ChainsawRand.nextComplex(-1, 1))
    println(ctfft(testCase).mkString(" "))
    println(DSP.FFT.Refs.FFT(testCase.toArray).mkString(" "))
  }
}

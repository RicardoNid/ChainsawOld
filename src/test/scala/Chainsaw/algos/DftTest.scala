package Chainsaw.algos

import Chainsaw.{ChainsawRand, RandomUtil}
import breeze.signal.fourierTr
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class DftTest extends AnyFlatSpec {

  behavior of "DftTest"

  val As = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))
  val Bs = (0 until 100).map(_ => ChainsawRand.nextComplexDV(10))

  it should "dft" in {
    As.foreach { data =>
      val mine = Dft.dft(data)
      val breeze = fourierTr.dvComplex1DFFT(data)
      assert(mine ~= breeze, s"\nmine  : $mine  \nbreeze: $breeze")
    }
  }

  it should "idft" in {
    As.foreach { data =>
      val N = BComplex(data.length.toDouble, 0.0)
      val mine = Dft.idft(Dft.dft(data)) / N
      val golden = data
      assert(mine ~= golden, s"\nmine  : $mine  \ngolden: $golden")
    }
  }

  it should "genericDft" in {

  }

  it should "rvdftByDouble" in {
    As.zip(Bs).foreach { case (a, b) =>
      val data0 = a.map(_.real)
      val data0AsComplex = data0.map(BComplex(_, 0.0))
      val data1 = b.map(_.real)
      val data1AsComplex = data1.map(BComplex(_, 0.0))

      val mine = Dft.rvdftByDouble(data0, data1)
      val golden = (Dft.dft(data0AsComplex), Dft.dft(data1AsComplex))
      println(mine._1)
      println(golden._1)
      assert((mine._1 ~= golden._1) && (mine._2 ~= golden._2), s"\nmine  : $mine  \ngolden: $golden")
    }
  }

  it should "rvidftByDouble" in {
    As.zip(Bs).foreach { case (a, b) =>
      val (data0, data1) = Dft.rvdftByDouble(a.map(_.real), b.map(_.real))
      val mine = Dft.rvidftByDouble(data0, data1)
      val golden = (Dft.idft(data0), Dft.idft(data1))
      val m0AsComplex = mine._1.map(BComplex(_, 0.0))
      val m1AsComplex = mine._2.map(BComplex(_, 0.0))
      assert((m0AsComplex ~= golden._1) && (m1AsComplex ~= golden._2), s"\nmine  : $mine  \ngolden: $golden")
    }
  }
}

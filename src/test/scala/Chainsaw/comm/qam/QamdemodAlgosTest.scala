package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.ceil

class QamdemodAlgosTest extends AnyFlatSpec {

  def isInt(value: Double) = value == ceil(value)

  def testSpecificBit(bitsAllocated: Int) = {
    val testUpperbound = Refs.getQAMValues(bitsAllocated).map(_.modulus).max
    val testCases = (0 until 200).map(_ => ChainsawRand.nextComplex(-testUpperbound, testUpperbound)).filterNot(complex => isInt(complex.real) || isInt((complex.imag)))
    val yours: Seq[Int] = testCases.map(Algos.qamdemod(_, bitsAllocated))
    val golden: Array[Int] = Refs.qamdemod(testCases.toArray, bitsAllocated)
    println(s"yours  ${yours.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    println(s"golden ${golden.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    val diff = golden.zip(yours).map { case (g, y) => (g - y).abs }
    if (!diff.forall(_ == 0) ){
      println(s"diff   ${diff.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
      println(s"diff types ${diff.map(_.toString.padToLeft(3, ' ')).distinct.mkString(" ")}")
      println(s"diff count ${diff.filter(_ != 0).size}")
    }
    assert(golden.mkString("") == yours.mkString(""))
  }

  "qamdemod" should "have correct output" in {
    Seq(1, 2, 3, 4, 5, 6, 7, 8).foreach(testSpecificBit)
  }

}

package Chainsaw.dsl

import Chainsaw._
import Chainsaw.dsl.ring._
import Chainsaw.dsl.transform.{Converter, LUT, Matrix, SPerm}
import breeze.math._

import scala.util.Random

object Examples {

  def main(args: Array[String]): Unit = {

    implicit val finiteField: FiniteRing = FiniteRing(2)
    implicit val intField: UIntRing = UIntRing(4)
    implicit val complexField: ComplexRing = ComplexRing(5, 10)

    complexField.selfTest()

    val data = Array.fill[FiniteInt](6)(0) ++ (0 until 128).map(_ => FiniteInt(Random.nextInt(2)))

    val conv = Matrix[FiniteInt](Array(
      Array[FiniteInt](1, 0, 0, 1, 1, 1, 1),
      Array[FiniteInt](1, 1, 0, 1, 1, 0, 1)))

    val sp16_16 = SPerm[FiniteInt](16, 16)

    val convert = new Converter(4, 1, finiteField, intField)

    val qamValue: Seq[BComplex] = (0 until 16).map(j => Complex(1,1))
    val qam16 = LUT[Complex](qamValue: _*)

    val dft2 = Matrix(Array(
      Array(1 + 0 * i, 1 + 0 * i),
      Array(1 + 0 * i, 1 + 0 * i)))

    //    val ofdm = (conv ⊗ (128, 1))
    //    val ofdm = sp16_16 ° (conv ⊗ (128, 1))
    //    val ofdm = sp16_16 ° (conv ⊗ (128, 1))
    //    val ofdm = (convert ⊗ 64) ° sp16_16 ° (conv ⊗ (128, 1))
    //    val ofdm = (qam16 ⊗ 64) ° (convert ⊗ 64) ° sp16_16 ° (conv ⊗ (128, 1))
    val ofdm = (dft2 ⊗ 32) ° (qam16 ⊗ 64) ° (convert ⊗ 64) ° sp16_16 ° (conv ⊗ (128, 1))
    ofdm.testOnce(data, targetThroughput = 0.25)

  }
}

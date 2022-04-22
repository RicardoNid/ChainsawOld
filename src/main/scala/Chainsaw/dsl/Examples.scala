package Chainsaw.dsl

import Chainsaw.dsl.field._
import Chainsaw.dsl.transform.base._
import Chainsaw.dsl.vectorspace.BasicVectorSpace

import scala.util.Random

object Examples {

  def main(args: Array[String]): Unit = {

    implicit val field = FiniteField2(2)
    implicit val vs = BasicVectorSpace(FiniteField2(2))

    implicit val intField = IntField(4)

    val data = Array.fill[FiniteInt](6)(0) ++ (0 until 128).map(_ => FiniteInt(Random.nextInt(2)))

    val conv = Matrix[FiniteInt](Array(
      Array[FiniteInt](1, 1, 1, 1, 0, 0, 1).reverse,
      Array[FiniteInt](1, 0, 1, 1, 0, 1, 1).reverse))

    val sp16_16 = SPermutation[FiniteInt](16, 16)

    val convert = new Converter(4, 1, field, intField)

    val ofdm = (convert ⊗ 64) ° sp16_16 ° (conv ⊗ (128, 1))
    println(ofdm(data).mkString("Array(", ", ", ")"))


  }
}

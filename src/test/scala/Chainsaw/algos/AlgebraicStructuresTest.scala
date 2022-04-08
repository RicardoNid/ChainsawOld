package Chainsaw.algos

import org.scalatest.flatspec.AnyFlatSpec

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import AlgebraicStructures._

class AlgebraicStructuresTest extends AnyFlatSpec {

  "test on finite field" should "work" in {

    implicit val field = FiniteField(7)
    val data: DenseVector[FFInt] = DenseVector(Array(1,2,3,4,5).map(FFInt(_)))

    println(data *:* data)
  }

  "test on Minplus" should "work" in {
    

  }

}

package Chainsaw.dsl.transform

import scala.reflect.ClassTag

case class TransformWithReuse[TIn, TOut](transform: Transform[TIn, TOut], duplicate: Duplicate)
                                        (implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut]) {
  def printType() = transform match {
    //    case lut: LUT[_] => println("is lut")
    case sp: SPermutation[_] => println("is s permutation")
    case p: Permutation[_] => println("is permutation matrix")
    case d: Diagonal[_] => println("is diagonal matrix")
    case m: Matrix[_] => println("is algebra matrix")
    case _ => println("no type")
  }

  import duplicate._

  def forceTransform(dataIn: Array[TIn]) = transform(dataIn).asInstanceOf[Array[TIn]]

  def apply(dataIn: Array[TIn]) = {
    dataIn.grouped(dataIn.length / parallel)
      .map(data => Array.iterate(data, iterative + 1)(forceTransform).last)
      .toArray.flatten.asInstanceOf[Array[TOut]]
  }
}



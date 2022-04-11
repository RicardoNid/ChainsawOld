package Chainsaw.dsl

abstract class Functor[T] {

  def transform(dataIn: Array[T]): Array[T]

  def +(that:Functor[T]) = Expression(Array(this, that))

}

package Chainsaw.dsl

abstract class Field[T] {

  def add(a: T, b: T): T

  def subtract(a: T, b: T): T

  def multiply(a: T, b: T): T

  def identity(a: T): T

  def zero: T

}

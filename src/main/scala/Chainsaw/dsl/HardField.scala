package Chainsaw.dsl

import spinal.core._

trait HardField[T, THard <: Data] {

  def fromConstant(constant: T): THard

  def undefined: THard

}

package Chainsaw.dsl

import spinal.core._

abstract class Signal[T, +THard <: Data](val value: T, val hardValue: THard) {
  def build[T, THard <: Data](value: T): Signal[T, THard]

  def buildHard[T, THard <: Data](value: THard): Signal[T, THard]
}





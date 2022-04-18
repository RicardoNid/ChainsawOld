package Chainsaw.dsl.field

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

abstract class Field[T] {

  def add(a: T, b: T): T

  def subtract(a: T, b: T): T

  def multiply(a: T, b: T): T

  def identity(a: T): T

  def zero: T

  def one: T

  val width:Int

  def addH(a: Bits, b: Bits): Bits

  def subtractH(a: Bits, b: Bits): Bits

  def multiplyH(a: Bits, b: Bits): Bits

  def identityH(a: Bits): Bits

  def zeroH: Bits

  def oneH: Bits

  def undefinedH: Bits

  def toHard(coeff: T): Bits

}

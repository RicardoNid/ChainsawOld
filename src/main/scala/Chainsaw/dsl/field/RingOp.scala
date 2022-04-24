package Chainsaw.dsl.field

import spinal.core._

trait RingOp[T] {

  def add(a: T, b: T): T

  def sub(a: T, b: T): T

  def mult(a: T, b: T): T

  def addH(a: Bits, b: Bits): Bits

  def subH(a: Bits, b: Bits): Bits

  def multH(a: Bits, b: Bits): Bits

  def idH(a: Bits): Bits

}

package Chainsaw.dsl.ring

import spinal.core._
import Chainsaw.dsl._

trait RingOp[T] {

  def add(a: T, b: T): T

  def sub(a: T, b: T): T

  def mult(a: T, b: T): T

  val addH: HardOp2
  val subH: HardOp2
  val multH: HardOp2

}

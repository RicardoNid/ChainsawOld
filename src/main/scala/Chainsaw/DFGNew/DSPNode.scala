package Chainsaw.DFGNew

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

abstract class DSPNode[T <: Data] {
  val impl: Seq[T] => T
  val implWidth: BitCount
  val name: String
  val delay: Cycle
  val exeTime: TimeNumber

  override def toString: String = name
}

/** Providing basic constructors
 */
class GeneralNode[T <: Data](implp: Seq[T] => T, widthp: BitCount, namep: String, delayp: Cycle, exeTimep: TimeNumber) extends DSPNode[T] {
  val impl: Seq[T] => T = implp
  override val implWidth: BitCount = widthp
  override val name: String = namep // TODO: implement reflection
  override val delay: Cycle = delayp
  override val exeTime: TimeNumber = exeTimep
}

object GeneralNode {
  def apply[T <: Data](implp: Seq[T] => T, widthp: BitCount, namep: String, delayp: Cycle, exeTimep: TimeNumber): GeneralNode[T] = new GeneralNode(implp, widthp, namep, delayp, exeTimep)

  def apply[T <: Data](namep: String, delayp: Cycle, exeTimep: TimeNumber): GeneralNode[T] = new GeneralNode((dataIns: Seq[T]) => dataIns.head, -1 bits, namep, delayp, exeTimep)
}

object VoidNode {
  def apply[T <: Data](name: String = "void") = GeneralNode[T](name, delayp = 0 cycle, exeTimep = 0 ns)
}

class InputNode[T <: Data](namev: String) extends DSPNode[T] {
  override val impl: Seq[T] => T = (dataIns: Seq[T]) => dataIns.head
  override val implWidth: BitCount = -1 bits
  override val name: String = namev
  override val delay: Cycle = 0 cycle
  override val exeTime: TimeNumber = 0 ns
}

object InputNode {
  def apply[T <: Data](namev: String): InputNode[T] = new InputNode(namev)
}

class OutputNode[T <: Data](namev: String) extends DSPNode[T] {
  override val impl: Seq[T] => T = (dataIns: Seq[T]) => dataIns.head
  override val implWidth: BitCount = -1 bits
  override val name: String = namev
  override val delay: Cycle = 0 cycle
  override val exeTime: TimeNumber = 0 ns
}

object OutputNode {
  def apply[T <: Data](namev: String): OutputNode[T] = new OutputNode(namev)
}
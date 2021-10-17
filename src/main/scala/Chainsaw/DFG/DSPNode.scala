package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

case class DSPHardware[T <: Data](impl: (Seq[T], GlobalCount) => Seq[T], inDegree: Int, outWidths: Seq[BitCount])

abstract class DSPNode[T <: Data] {
  val hardware: DSPHardware[T]
  val name: String
  val delay: Int
  val exeTime: Double

  def copy(newName: String): DSPNode[T]

  override def toString: String = name
}

/** Providing basic constructors
 */
class GeneralNode[T <: Data](implp: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber) extends DSPNode[T] {
  override val hardware: DSPHardware[T] = implp
  override val name: String = namep // TODO: implement reflection
  override val delay: Int = delayp.toInt
  override val exeTime: Double = exeTimep.toDouble

  // FIXME: that's weird
  override def copy(newName: String): DSPNode[T] = new GeneralNode(hardware, newName, delay cycles, exeTime sec)
}

object GeneralNode {
  def apply[T <: Data](hardware: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber): GeneralNode[T] = new GeneralNode(hardware, namep, delayp, exeTimep)

  def apply[T <: Data](namep: String, delayp: CyclesCount, exeTimep: TimeNumber): GeneralNode[T] = new GeneralNode(Operators.Line[T](), namep, delayp, exeTimep)

  def apply[T <: Data](namep: String, delayp: CyclesCount, exeTimep: TimeNumber, width: BitCount): GeneralNode[T] = new GeneralNode(Operators.Line[T](width), namep, delayp, exeTimep)
}

object VoidNode {
  def apply[T <: Data](name: String = "void") = GeneralNode[T](name, delayp = 0 cycles, exeTimep = 0 ns)
}

class InputNode[T <: Data](namev: String) extends DSPNode[T] {
  override val hardware = Operators.Line[T]()
  override val name: String = namev
  override val delay: Int = 0
  override val exeTime: Double = 0

  override def copy(newName: String): DSPNode[T] = new InputNode[T](newName)
}

object InputNode {
  def apply[T <: Data](namev: String): InputNode[T] = new InputNode(namev)
}

class OutputNode[T <: Data](namev: String) extends DSPNode[T] {
  override val hardware = Operators.Line[T]()
  override val name: String = namev
  override val delay: Int = 0
  override val exeTime: Double = 0

  override def copy(newName: String): DSPNode[T] = new OutputNode[T](newName)
}

object OutputNode {
  def apply[T <: Data](namev: String): OutputNode[T] = new OutputNode(namev)
}
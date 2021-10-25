package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class DSPHardware[T](val impl: (Seq[T], GlobalCount) => Seq[T], val inDegree: Int, val outWidths: Seq[BitCount] = Seq(-1 bit))

object DSPHardware {
  def apply[T](impl: (Seq[T], GlobalCount) => Seq[T], inDegree: Int, outWidths: Seq[BitCount]): DSPHardware[T] = new DSPHardware(impl, inDegree, outWidths)
}

case class BinaryHardware[T](op: (T, T) => T, width: BitCount = -1 bits)
  extends DSPHardware[T](impl = (dataIns: Seq[T], _: GlobalCount) => Seq(op(dataIns(0), dataIns(1))), inDegree = 2, outWidths = Seq(width))

class BinaryNode[T](op: (T, T) => T, width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends GeneralNode[T](BinaryHardware(op, width), name, delay, exeTime)

object BinaryNode {
  def apply[T](op: (T, T) => T, name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): BinaryNode[T] = new BinaryNode(op, width, name, delay, exeTime)
}

abstract class DSPNode[T] {
  val hardware: DSPHardware[T]
  val name: String
  val delay: Int
  val exeTime: Double

  def copy(newName: String): DSPNode[T]

  override def toString: String = name
}

/** Providing basic constructors
 */
class GeneralNode[T](implp: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber) extends DSPNode[T] {
  override val hardware: DSPHardware[T] = implp
  override val name: String = namep // TODO: implement reflection
  override val delay: Int = delayp.toInt
  override val exeTime: Double = exeTimep.toDouble

  // FIXME: that's weird
  override def copy(newName: String): DSPNode[T] = new GeneralNode(hardware, newName, delay cycles, exeTime sec)
}

object GeneralNode {
  def apply[T](hardware: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber): GeneralNode[T] = new GeneralNode(hardware, namep, delayp, exeTimep)

  def apply[T](namep: String, delayp: CyclesCount, exeTimep: TimeNumber): GeneralNode[T] = new GeneralNode(Operators.Line[T](), namep, delayp, exeTimep)

  def apply[T](namep: String, delayp: CyclesCount, exeTimep: TimeNumber, width: BitCount): GeneralNode[T] = new GeneralNode(Operators.Line[T](width), namep, delayp, exeTimep)
}

object VoidNode {
  def apply[T](name: String = "void") = GeneralNode[T](name, delayp = 0 cycles, exeTimep = 0 ns)
}

class InputNode[T](namev: String) extends DSPNode[T] {
  override val hardware = Operators.Line[T]()
  override val name: String = namev
  override val delay: Int = 0
  override val exeTime: Double = 0

  override def copy(newName: String): DSPNode[T] = new InputNode[T](newName)
}

object InputNode {
  def apply[T](namev: String): InputNode[T] = new InputNode(namev)
}

class OutputNode[T](namev: String) extends DSPNode[T] {
  override val hardware = Operators.Line[T]()
  override val name: String = namev
  override val delay: Int = 0
  override val exeTime: Double = 0

  override def copy(newName: String): DSPNode[T] = new OutputNode[T](newName)
}

object OutputNode {
  def apply[T](namev: String): OutputNode[T] = new OutputNode(namev)
}

class ConstantNode[T](implp: DSPHardware[T], namep: String) extends DSPNode[T] {
  override val hardware: DSPHardware[T] = implp
  override val name: String = namep // TODO: implement reflection
  override val delay: Int = 0
  override val exeTime: Double = 0.0

  override def copy(newName: String): DSPNode[T] = new ConstantNode(hardware, newName)
}

object ConstantNode {
  def apply[T](implp: DSPHardware[T], namep: String): ConstantNode[T] = new ConstantNode(implp, namep)

  def apply[THard <: Data, TSoft](name: String, constant: TSoft, width: BitCount)(implicit converter: (TSoft, BitCount) => THard): ConstantNode[THard] = {
    val hardware = DSPHardware(
      impl = (_: Seq[THard], _: GlobalCount) => Seq(converter(constant, width)),
      inDegree = 1, outWidths = Seq(width))
    ConstantNode(hardware, name)
  }
}
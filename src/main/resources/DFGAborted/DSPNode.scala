package Chainsaw.DFGAborted

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

abstract class DSPNode {

  def name: String = ""

  def impl(dataIn: Seq[Bits]): Bits

  def implWidth: Int = -1

  def delay: Int

  def executionTime: Double

  def -(that: DSPNode) = Constraint(this, that, 0)
}

class AbstractNode(delayv: Int, executionTimev: Double, name: String) extends DSPNode {
  override def impl(dataIn: Seq[Bits]): Bits = dataIn.head // output node has only 1 source, dataIn.size == 1

  override def delay: Int = delayv

  override def executionTime: Double = executionTimev

  override def toString: String = name
}

object AbstractNode {
  def apply(delayv: Int, executionTimev: Double, name: String = "tmp"): AbstractNode = new AbstractNode(delayv, executionTimev, name)
}

class VoidNode(namev: String = "tmp") extends AbstractNode(0,0,namev)

object VoidNode {
  def apply(namev: String): VoidNode = new VoidNode(namev)
}

class InputNode extends VoidNode

class OutputNode extends VoidNode

object InputNode {
  def apply(): InputNode = new InputNode()
}

object OutputNode {
  def apply(): OutputNode = new OutputNode()
}

object TmpNode {
  def apply(): AbstractNode = new AbstractNode(0, 0, "tmp")
}
package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

abstract class DSPNode {

  def impl(dataIn: Seq[Bits]): Bits

  def implWidth: Int = -1

  def delay: Int

  def executionTime: Double
}

class InputNode extends DSPNode {
  override def impl(dataIn: Seq[Bits]): Bits = dataIn.head // input node has only 1 source, dataIn.size == 1

  override def delay: Int = 0

  override def executionTime: Double = 0
}

object InputNode {
  def apply(): InputNode = new InputNode()
}

class OutputNode extends DSPNode {
  override def impl(dataIn: Seq[Bits]): Bits = dataIn.head // output node has only 1 source, dataIn.size == 1

  override def delay: Int = 0

  override def executionTime: Double = 0
}

object OutputNode {
  def apply(): OutputNode = new OutputNode()
}

class PrueNode(delayv: Int, executionTimev: Double) extends DSPNode {
  override def impl(dataIn: Seq[Bits]): Bits = dataIn.head // output node has only 1 source, dataIn.size == 1

  override def delay: Int = delayv

  override def executionTime: Double = executionTimev
}

object PrueNode {
  def apply(delayv: Int, executionTimev: Double): PrueNode = new PrueNode(delayv, executionTimev)
}

object TmpNode{
  def apply(): PrueNode = new PrueNode(0,0)
}
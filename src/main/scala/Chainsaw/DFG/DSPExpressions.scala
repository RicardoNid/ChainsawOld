package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/** A linear transformation on latency, x => scale * x + shift
 */
case class LatencyTrans(scale: Int, shift: Int) {
  def trans(original: Int) = scale * original + shift
}

case class DSPAssignment[T <: Data](sources: Seq[DSPNode[T]], delays: Seq[Double], var target: DSPNode[T]) {
  def >=>(that: DSPNode[T]) = {
    target = that
    this
  }
}

object DSPAssignment {
  def apply[T <: Data](sources: DSPNode[T], delay: Double, target: DSPNode[T]): DSPAssignment[T] = new DSPAssignment(Seq(sources), Seq(delay), target)
}

/** a path containing interleaving nodes and edge, these nodes should be SISO, as port indices are not given
 *
 * @example a >> 1 >> b >> 2 >> c, a >> b equals a >> 0 >> b
 */
case class DSPPath[T <: Data](nodes: ArrayBuffer[DSPNode[T]], delays: ArrayBuffer[Double]) {

  def >>(that: DSPNode[T]): DSPPath[T] = {
    nodes += that
    if (nodes.size != delays.size + 1) delays += 0
    this
  }

  def >>(that: Int): DSPPath[T] = {
    delays += that
    this
  }
}

/** source - target <= value, without any retiming, r(V) = r(U) = 0, so when value < 0, retiming is needed
 */
case class DSPConstraint[T <: Data](target: DSPNode[T], source: DSPNode[T], value: Int) {
  def <=(value: Int): DSPConstraint[T] = DSPConstraint(target, source, value = value)

  def >=(value: Int): DSPConstraint[T] = DSPConstraint(source, target, value = -value)
}

case class DSPNodeWithOrder[T <: Data](node: DSPNode[T], order: Int)

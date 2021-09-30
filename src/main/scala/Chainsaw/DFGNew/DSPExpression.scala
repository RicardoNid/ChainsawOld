package Chainsaw.DFGNew

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

case class DSPExpression[T <: Data](sources: Seq[DSPNode[T]], delays: Seq[Int], var target: DSPNode[T]){
  def >=>(that: DSPNode[T]) = {
    target = that
    this
  }
}

object DSPExpression {
  def apply[T <: Data](sources: DSPNode[T], delay: Int, target: DSPNode[T]): DSPExpression[T] = new DSPExpression(Seq(sources), Seq(delay), target)
}

case class DSPPath[T <: Data](nodes: ArrayBuffer[DSPNode[T]], delays: ArrayBuffer[Int]){
  def >>(that: DSPNode[T]) = {
    nodes += that
    this
  }

  def >>(that: Int) = {
    delays += that
    this
  }
}

case class DSPConstraint[T <: Data](target: DSPNode[T], source: DSPNode[T], value: Int) {
  def <=(value: Int) = DSPConstraint(target, source, value = value)
}
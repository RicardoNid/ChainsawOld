package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import spinal.core.{BitCount, IntToBuilder}

trait Foldable[T <: Data] {
  def fold(sources: Seq[DSPNode[T]]): DSPNode[T] = sources.head
}

trait Transform {
  def latencyTransformations: Seq[LatencyTrans]
}

trait DFGGen[T <: Data] {
  def getGraph: DFGGraph[T]

  def latency: Int

  def getGraphAsNode(dataReset: Boolean = false)(implicit holderProvider: HolderProvider[T]): DSPNode[T]
}

trait NodeComponent[T <: Data] {
  val dataIn: Vec[T]
  val dataOut: Vec[T]
}

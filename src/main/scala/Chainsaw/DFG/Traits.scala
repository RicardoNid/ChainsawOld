package Chainsaw.DFG

import spinal.core.{BitCount, IntToBuilder}

trait Foldable[T] {
  def fold(sources: Seq[DSPNode[T]]): DSPNode[T] = sources.head
}

trait Transform {
  def latencyTransformations: Seq[LatencyTrans]
}

trait DFGGen[T] {
  def getGraph: DFGGraph[T]

  def latency: Int

  def getGraphAsNode(implicit holderProvider: BitCount => T): DSPNode[T]
}

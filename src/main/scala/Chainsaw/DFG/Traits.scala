package Chainsaw.DFG

import spinal.core._

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

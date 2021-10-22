package Chainsaw.DFG

import spinal.core._

trait Foldable[T <: Data] {
  def fold(sources: Seq[DSPNode[T]]): DSPNode[T]
}

trait Transform {
  def latencyTrans: LatencyTrans
}

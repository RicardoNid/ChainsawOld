package Chainsaw.DFG

import spinal.core._

trait Foldable[T] {
  def fold(sources: Seq[DSPNode[T]]): DSPNode[T]
}

trait Transform {
  def latencyTrans: LatencyTrans
}

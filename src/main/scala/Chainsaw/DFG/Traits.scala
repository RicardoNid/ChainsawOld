package Chainsaw.DFG

trait Foldable[T] {
  def fold(sources: Seq[DSPNode[T]]): DSPNode[T] = sources.head
}

trait Transform {
  def latencyTransformations: Seq[LatencyTrans]
}

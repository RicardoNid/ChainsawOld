package Chainsaw.DFG

import Chainsaw._

import scala.collection.mutable
import scala.language.postfixOps

class DFGImplSoft[T](dfg: DFGGraph[T])(implicit val zero: T) {
  require(dfg.isForwarding) // or, delay/timing concerns, and we can't implement that
  implicit val currentDFG: DFGGraph[T] = dfg

  val vertexSeq: Seq[DSPNode[T]] = dfg.vertexSeq
  val inputNodes: Seq[DSPNode[T]] = dfg.inputNodes
  val outputNodes: Seq[DSPNode[T]] = dfg.outputNodes

  def implVertex(target: DSPNode[T], signalMap: Map[DSPNode[T], Seq[T]]): Seq[T] = {
    val dataIns: Seq[T] = target.incomingEdges.map(edge => signalMap(edge.source)(edge.outOrder))
    target.hardware.impl(dataIns, null) // using null as a SpinalHDL literal will make the implementation fail
  }

  def implForwarding: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    logger.info("implementing software DFG by algo for forwarding DFG")
    require(inputNodes.size == dataIns.size, "input size mismatch")
    val signalMap = mutable.Map[DSPNode[T], Seq[T]]()
    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap += node -> Seq(bits) }

    def implemented = signalMap.keys.toSeq

    def remained = vertexSeq.diff(implemented)

    def nextStageNodes = remained.filter(_.sources.forall(implemented.contains(_)))

    while (nextStageNodes.nonEmpty) {
      nextStageNodes.foreach { target =>
        val rets = implVertex(target, signalMap.toMap)
        signalMap += target -> rets
      }
    }
    outputNodes.flatMap(signalMap(_))
  }
}
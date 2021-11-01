package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.collection.mutable
import scala.language.postfixOps

class DFGImplSoft[T](dfg: DFGGraph[T])(implicit val zero: T) {

  implicit val currentDFG = dfg

  val vertexSeq: Seq[DSPNode[T]] = dfg.vertexSeq
  val inputNodes: Seq[DSPNode[T]] = dfg.inputNodes
  val outputNodes: Seq[DSPNode[T]] = dfg.outputNodes

  def implVertex(target: DSPNode[T], signalMap: Map[DSPNode[T], Seq[T]]): Seq[T] = {
    val dataIns: Seq[T] = target.incomingEdges.map(edge => signalMap(edge.source)(edge.outOrder))
    target.hardware.impl(dataIns, null) // FIXME: the GlobalCount will make the implementation fail
  }

  def implForwarding: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    logger.info("implementing DFG by algo for forwarding DFG")
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

object DFGImplSoft {
  def main(args: Array[String]): Unit = {
    val add = (a: Int, b: Int) => a + b
    val addGraph = DFGGraph[Int]("addGraph")
    val addNode = BinaryNode(add, "add")
    addGraph.addVertex(addNode)
    addGraph.setInput(addNode, 0)
    addGraph.setInput(addNode, 1)
    addGraph.setOutput(addNode)
    implicit val zero = 0
    val addFunction = new DFGImplSoft(addGraph).implForwarding
    println(addFunction(Seq(1, 2)))
  }
}
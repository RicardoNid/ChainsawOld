package Chainsaw.DFG

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class DFGImpl[T <: Data](dfg: DFGGraph[T]) {
  implicit def currentDFG = dfg

  implicit val holderProvider = dfg.holderProvider

  // attributes tobe used
  val vertexSeq = dfg.vertexSeq
  val globalLcm = dfg.globalLcm
  val inputNodes = dfg.inputNodes
  val outputNodes = dfg.outputNodes

  def initGlobalCount() = {
    val globalCounter = CounterFreeRun(globalLcm)
    globalCounter.value.setName("globalCounter")
    GlobalCount(globalCounter.value)
  }

  def implVertex(target: DSPNode[T], signalMap: Map[DSPNode[T], Seq[T]])(implicit globalCount: GlobalCount) = {
    val dataIns: Seq[Seq[DSPEdge[T]]] = target.incomingEdges.groupBy(_.inOrder).toSeq.sortBy(_._1).map(_._2)
    val dataInsOnPorts = dataIns.zipWithIndex.map { case (dataInsOnePort, i) => // combine dataIns at the same port by a mux
      val dataCandidates: Seq[T] = dataInsOnePort.map(edge => edge.hardware(signalMap(edge.source), edge.weight.toInt).apply(edge.outOrder))
      // showing mux infos
      //        println(s"mux to target $target:\n" +
      //          s"${dataInsOnePort.map(_.schedules.mkString(" ")).mkString("\n")}")
      val mux = DFGMUX[T](dataInsOnePort.map(_.schedules))
      val succeed = Try(mux.impl(dataCandidates, globalLcm))
      succeed match {
        case Failure(exception) => printlnRed(s"collision between:\n${dataInsOnePort.map(_.symbol).mkString("\n")}")
          mux.impl(dataCandidates, globalLcm)
        case Success(value) => value
      }
    }
    // implement target using dataIns from different ports
    target.hardware.impl(dataInsOnPorts, globalCount)
  }

  // implement a recursive graph
  def implRecursive: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    printlnGreen("implementing DFG by algo for recursive DFG")
    val signalMap: Map[DSPNode[T], Seq[T]] = vertexSeq.map{node => // a map to connect nodes with their outputs(placeholder)
      printlnRed(s"vertex $node: ${node.hardware.outWidths.mkString(" ")}")
      node -> node.hardware.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i))}.toMap

    implicit val globalCount = initGlobalCount()

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }
    vertexSeq.diff(inputNodes).foreach{target =>
      val rets = implVertex(target, signalMap)
      val placeholders = signalMap(target)
      placeholders.zip(rets).foreach { case (placeholder, ret) => placeholder := ret.resized }
      if (placeholders.size == 1) placeholders.head.setName(target.name) // name these signals
      else placeholders.zipWithIndex.foreach { case (placeholder, i) => placeholder.setName(s"${target}_$i") }
    }
    outputNodes.map(signalMap(_)).flatten
  }

  def implForwarding: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    printlnGreen("implementing DFG by algo for forwarding DFG")
    val signalMap = mutable.Map[DSPNode[T], Seq[T]]()
    implicit val globalCount = initGlobalCount()
    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap += node -> Seq(bits) }

    def implemented = signalMap.keys.toSeq

    def remained = vertexSeq.diff(implemented)

    def nextStageNodes = remained.filter(_.sources.forall(implemented.contains(_)))

    while (nextStageNodes.nonEmpty) {
      nextStageNodes.foreach { target =>
        val rets = implVertex(target, signalMap.toMap)
        signalMap += target -> rets
        if (target.hardware.outWidths.size == 1) rets.head.setName(target.name)
        else rets.zipWithIndex.foreach { case (ret, i) => ret.setName(s"${target}_$i") }
      }
    }
    outputNodes.map(signalMap(_)).flatten
  }

  def impl = if(dfg.isRecursive) implRecursive else implForwarding

}

package Chainsaw.DFG

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

class DFGImpl[T <: Data](dfg: DFGGraph[T]) {

  implicit def currentDFG = dfg
  val vertexSeq = dfg.vertexSeq
  val holderProvider = dfg.holderProvider
  val globalLcm = dfg.globalLcm
  val inputNodes = dfg.inputNodes
  val outputNodes = dfg.outputNodes

  // implement a recursive graph
  def implRecursive: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    val signalMap: Map[DSPNode[T], Seq[T]] = vertexSeq.map(node => // a map to connect nodes with their outputs(placeholder)
      node -> node.hardware.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i))).toMap

    // create the global counter
    val globalCounter = CounterFreeRun(globalLcm)
    globalCounter.value.setName("globalCounter")
    implicit val globalCount = GlobalCount(globalCounter.value)

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }

    vertexSeq.diff(inputNodes).foreach { target =>
      val dataIns: Seq[Seq[DSPEdge[T]]] = target.incomingEdges.groupBy(_.inOrder).toSeq.sortBy(_._1).map(_._2)
      val dataInsOnPorts = dataIns.zipWithIndex.map { case (dataInsOnePort, i) => // combine dataIns at the same port by a mux
        val dataCandidates: Seq[T] = dataInsOnePort.map(edge => edge.hardware(signalMap(edge.source), edge.weight.toInt).apply(edge.outOrder))
        // showing mux infos
        //        println(s"mux to target $target:\n" +
        //          s"${dataInsOnePort.map(_.schedules.mkString(" ")).mkString("\n")}")
        val mux = DFGMUX[T](dataInsOnePort.map(_.schedules))(dfg.holderProvider)
        mux.impl(dataCandidates, globalLcm)
      }
      // implement target using dataIns from different ports
      val placeholders = signalMap(target)
      val rets = target.hardware.impl(dataInsOnPorts, globalCount)
      placeholders.zip(rets).foreach { case (placeholder, ret) => placeholder := ret.resized }
      if (placeholders.size == 1) placeholders.head.setName(target.name) // name these signals
      else placeholders.zipWithIndex.foreach { case (placeholder, i) => placeholder.setName(s"${target}_$i") }
    }
    outputNodes.map(signalMap(_)).flatten
  }

}

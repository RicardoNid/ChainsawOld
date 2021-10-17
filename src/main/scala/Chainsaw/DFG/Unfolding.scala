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

class Unfolding[T <: Data](dfg: DFGGraph[T], unfoldingFactor: Int) {

  // TODO: considering fold a dfg by 2 and then unfold it by 4
  // currently, we do not  allow this
  if (!dfg.hasNoMux) require(dfg.globalLcm % unfoldingFactor == 0) // when there's mux and we use unfolding

  /** Preprocess the dfg to separate delay and mux
   *
   */
  def preprocessed = {
    val preprocessedDFG = dfg.clone().asInstanceOf[DFGGraph[T]]
    implicit def currentDFG = preprocessedDFG
    preprocessedDFG.foreachEdge { edge =>
      val source = edge.source
      val target = edge.target
      if (!edge.hasNoMux && edge.weight != 0) {
        val virtualNode = GeneralNode[T](s"virtual_from_${edge.source}", 0 cycles, 0 ns, target.hardware.outWidths(edge.outOrder))
        // separate mux and delay
        val delayEdge = DefaultDelay[T](Seq(Schedule(0, 1)), edge.outOrder, 0)
        val muxEdge = DefaultDelay[T](edge.schedules, 0, edge.inOrder)
        preprocessedDFG.addVertex(virtualNode)
        preprocessedDFG.addEdge(source, virtualNode, delayEdge)
        preprocessedDFG.addEdge(virtualNode, target, muxEdge)
        preprocessedDFG.setEdgeWeight(delayEdge, edge.weight)
        preprocessedDFG.setEdgeWeight(muxEdge, 0)
        preprocessedDFG.removeEdge(edge)
      }
    }
    printlnGreen(preprocessedDFG)
    preprocessedDFG
  }

  def unfolded = {
    implicit val preprocessedDFG = preprocessed
    val unfoldedDFG = DFGGraph[T](dfg.holderProvider)
    val nodeMap = preprocessedDFG.vertexSeq.map(vertex => vertex -> (0 until unfoldingFactor).map(i => vertex.copy(s"${vertex.name}_unfolded_$i"))).toMap
    preprocessedDFG.foreachEdge { edge =>
      val w = edge.weightWithSource
      val sources = nodeMap(edge.source)
      val targets = nodeMap(edge.target)

      if (edge.hasNoMux) {
        (0 until unfoldingFactor).foreach { i =>
          val j = (i + w) % unfoldingFactor
          val unfoldedDelay = (i + w) / unfoldingFactor
          val unfoldedEdge = DefaultDelay[T](Seq(Schedule(0,1)), edge.outOrder, edge.inOrder)
          unfoldedDFG.addVertex(sources(i))
          unfoldedDFG.addVertex(targets(j))
          unfoldedDFG.addEdge(sources(i), targets(j), unfoldedEdge)
          unfoldedDFG.setEdgeWeight(unfoldedEdge, unfoldedDelay)
        }
      } else {
        require(edge.weight == 0)
        (0 until unfoldingFactor).foreach { i =>
          val unfoldedSchedules = edge.schedules.filter(_.time % unfoldingFactor == i)
            .map(schedule => Schedule(schedule.time / unfoldingFactor, schedule.period / unfoldingFactor))
          if(unfoldedSchedules.nonEmpty){
            val unfoldedEdge = DefaultDelay[T](unfoldedSchedules, edge.outOrder, edge.inOrder)
            unfoldedDFG.addVertex(sources(i))
            unfoldedDFG.addVertex(targets(i))
            unfoldedDFG.addEdge(sources(i), targets(i), unfoldedEdge)
            unfoldedDFG.setEdgeWeight(unfoldedEdge, 0)
          }
        }
      }
    }
    unfoldedDFG
  }
}

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

  require(dfg.hasNoMux) // TODO: implement unfolding for MUX
  val unfoldedDFG = DFGGraph[T](dfg.holderProvider)

  val nodeMap = dfg.vertexSeq.map(vertex => vertex -> (0 until unfoldingFactor).map(i => vertex.copy(s"${vertex.name}_unfolded_$i"))).toMap
  println(nodeMap.mkString("\n"))
  dfg.foreachEdge { edge =>
    val w = dfg.getEdgeWeight(edge).toInt
    val sources = nodeMap(dfg.getEdgeSource(edge))
    val targets = nodeMap(dfg.getEdgeTarget(edge))
    (0 until unfoldingFactor).foreach { i =>
      val j = (i + w) % unfoldingFactor
      val wPrime = (i + w) / unfoldingFactor
      unfoldedDFG.addPath(sources(i) >> wPrime >> targets(j))
    }
  }

  def unfolded = unfoldedDFG
}

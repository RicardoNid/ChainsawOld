package Chainsaw.DFG

import org.jgrapht._
import org.jgrapht.alg.cycle.HowardMinimumMeanCycle
import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/** Create Gd from dfg
 */
class CriticalPathAlgo[T](dfg: DFGGraph[T]) {

  // building the CriticalPathGraph for following functions
  val graph = dfg.clone().asInstanceOf[DFGGraph[T]]
  // step1: expose delays
  graph.foreachVertex { vertex =>
    val edgeGroups = dfg.outgoingEdgesOf(vertex).toSeq.groupBy(_.outOrder).toSeq.sortBy(_._1)
    edgeGroups.foreach { case (outputPort, edges) =>
      if (edges.nonEmpty) {
        val delays = edges.map(dfg.getEdgeWeight).map(_.toInt)
        val length = delays.max
        if (length > 0) {
          val temps: Seq[GeneralNode[T]] = (0 until length).map(i => VoidNode[T](s"${vertex.name}.${outputPort}_delay${i + 1}"))
          val starts = vertex +: temps.init
          val ends = temps
          starts.zip(ends).foreach { case (start, end) => graph.addPath(start >> 1 >> end) }

          edges.zip(delays).foreach { case (edge, delay) =>
            if (delay > 0) {
              graph.addPath(temps(delay - 1) >> 0 >> dfg.getEdgeTarget(edge))
              graph.removeEdge(edge)
            }
          }
        }
      }
    }
  }
  println(graph)
  // step2: constructing graph for BellmanFord
  val starts = ArrayBuffer[DSPNode[T]]()
  val delays = starts // we specify the delay unit by the node it drives
  val ends = ArrayBuffer[DSPNode[T]]()
  graph.foreachEdge { edge =>
    if (graph.getEdgeWeight(edge) > 0) {
      starts += graph.getEdgeTarget(edge)
      ends += graph.getEdgeSource(edge)
      graph.removeEdge(edge) // removing edges with delay(break loops)
    } else graph.setEdgeWeight(edge, -graph.getEdgeTarget(edge).exeTime) // move the weight from vertices to edge
  }
  println(graph)
  // step3: running BellmanFord on graph to get
  val shortestPathAlgo = new alg.shortestpath.BellmanFordShortestPath(graph)

  // following functions are based on the graph

  /** Number of delay units in tht DFG
   */
  def delaysCount = delays.size

  /** Matrix(i,j) = longest path of delays(i) -> delays(j)
   */
  def weightMatix = Seq.tabulate(delays.size, delays.size)((i, j) => -shortestPathAlgo.getPathWeight(starts(i), ends(j)) + starts(i).exeTime)

  def criticalPath = {
    val index = weightMatix.flatten.indexOf(weightMatix.flatten.max)
    val i = index / delays.size
    val j = index % delays.size
    shortestPathAlgo.getPath(starts(i), ends(j))
  }

  def criticalPathLength = weightMatix.flatten.max

  def iterationBound = {
    val Gd = DFGGraph[T]()
    Seq.tabulate(starts.size, ends.size)((i, j) => if (weightMatix(i)(j) >= 0) Gd.addPath(delays(i) >> -weightMatix(i)(j) >> delays(j)))
    println(Gd)
    // step4: running MCM on Gd
    val mcmAlgo = new HowardMinimumMeanCycle(Gd)
    -mcmAlgo.getCycleMean
  }
}

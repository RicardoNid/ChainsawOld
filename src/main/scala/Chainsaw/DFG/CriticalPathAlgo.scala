package Chainsaw.DFG

import org.jgrapht._
// import org.jgrapht.alg.cycle.HowardMinimumMeanCycle
import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/** providing algorithms on analysis of critical paths
  *
  * @see
  *   ''VLSI Digital Signal Processing Systems: Design and Implementation'' Chap2.4.2, the graph G_d
  */
class CriticalPathAlgo[T <: Data](dfg: DFGGraph[T]) {

  implicit val currentDFG: DFGGraph[T] = dfg // original graph
  val graph: DFGGraph[T]               = dfg.clone().asInstanceOf[DFGGraph[T]] // new graph

  // step1: expose delays(make them explicit nodes in the graph)
  graph.foreachVertex { vertex => // processing nodes one at a time
    val edgeGroups: Seq[(Int, Seq[DSPEdge[T]])] = vertex.outgoingEdges // for all the outgoing edges from a node
      .groupBy(_.outOrder)
      .toSeq // group by output port, an element is portIndex -> edges from this port

    edgeGroups.foreach { case (outputPort, edges) =>
      if (edges.nonEmpty) {
        val delays = edges.map(_.delay)
        val length = delays.max // registers needed
        if (length > 0) { // refactor multiple delay lines by single delay line and multiple nodes on it
          val temps: Seq[VirtualNode[T]] = // virtual nodes working as passthroughs
            (0 until length).map(i => VirtualNode[T](s"${vertex.name}.${outputPort}_delay${i + 1}"))
          // end-to-end connection
          val starts = vertex +: temps.init
          val ends   = temps
          starts.zip(ends).foreach { case (start, end) => graph.addPath(start >> 1 >> end) }
          // "move" the edges
          edges.zip(delays).foreach { case (edge, delay) =>
            if (delay > 0) {
              graph.addPath(temps(delay - 1) >> 0 >> edge.target)
              graph.removeEdge(edge)
            }
          }
        }
      }
    }
  }

  // step2: constructing graph for BellmanFord
  val starts = ArrayBuffer[DSPNode[T]]()
  val delays = starts // we specify the delay unit by the node it drives
  val ends   = ArrayBuffer[DSPNode[T]]()
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
    val i     = index / delays.size
    val j     = index % delays.size
    shortestPathAlgo.getPath(starts(i), ends(j))
  }

  def criticalPathLength = weightMatix.flatten.max

  def iterationBound = {
    val Gd = DFGGraph[T](s"${graph.name}_critical")
    Seq.tabulate(starts.size, ends.size)((i, j) => if (weightMatix(i)(j) >= 0) Gd.addPath(delays(i) >> -weightMatix(i)(j) >> delays(j)))
    println(Gd)
    // step4: running MCM on Gd
    // FIXME: this algo needs JGraphT 1.5.1, thus needs JVM 11
    //    val mcmAlgo = new HowardMinimumMeanCycle(Gd)
    //    -mcmAlgo.getCycleMean
    0.0
  }
}

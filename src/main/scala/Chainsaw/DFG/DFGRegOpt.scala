package Chainsaw.DFG

import spinal.core._

/** providing algorithms on optimizing registers and memories
 *
 * @param dfg
 * @param dataReset
 * @param holderProvider
 * @tparam T
 */
class DFGRegOpt[T <: Data](dfg: DFGGraph[T]) {

  implicit val referenceDFG: DFGGraph[T] = dfg

  /** merge delay lines from the same signal
   */
  def getRegMergedDFG: DFGGraph[T] = {
    val graph: DFGGraph[T] = dfg.clone().asInstanceOf[DFGGraph[T]] // new graph
    graph.foreachVertex { vertex => // processing nodes one at a time
      val edgeGroups: Seq[(Int, Seq[DSPEdge[T]])] = vertex.outgoingEdges // for all the outgoing edges from a node
        .groupBy(_.outOrder).toSeq // group by output port, an element is portIndex -> edges from this port

      edgeGroups.foreach { case (outputPort, edges) =>
        if (edges.nonEmpty) {
          val delays = edges.map(_.delay)
          val length = delays.max // registers needed
          if (length > 0) { // refactor multiple delay lines by single delay line and multiple nodes on it
            val temps: Seq[VirtualNode[T]] = // virtual nodes working as passthroughs
              (0 until length).map(i => VirtualNode[T](s"${vertex.name}.${outputPort}_delay${i + 1}"))
            // end-to-end connection
            val starts = vertex +: temps.init
            val ends = temps
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
    graph
  }

  /** minimize reg number by lifecycle method
   *
   */
  @deprecated // TODO: is there a "todo" annotation(instead of deprecated)
  def getRegMinimizedDFG = {}

}

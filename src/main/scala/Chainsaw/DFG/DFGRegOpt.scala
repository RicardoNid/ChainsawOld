package Chainsaw.DFG

import spinal.core._

/** providing algorithms on optimizing registers and memories
 *
 */
class DFGRegOpt[T <: Data](dfg: DFGGraph[T]) {

  implicit val referenceDFG: DFGGraph[T] = dfg

  /** merge delay lines from the same signal
   */
  def getRegMergedDFG: DFGGraph[T] = {
    val graph: DFGGraph[T] = dfg.clone().asInstanceOf[DFGGraph[T]] // new graph

    graph.foreachVertex { vertex => // processing nodes one at a time

      vertex match {
        case constant: ConstantNode[_] => constant.outgoingEdges.foreach(graph.setEdgeWeight(_, 0)) // remove the delays from constantNode
        case vertex: DeviceNode[_] => // merge the delays from deviceNode
          val edgeGroups: Seq[(Int, Seq[DSPEdge[T]])] = vertex.outgoingEdges // for all the outgoing edges from a node
            .groupBy(_.outOrder).toSeq // group by output port, an element is portIndex -> edges from this port

          edgeGroups.foreach { case (outputPort, edges) => // processing ports one at a time
            if (edges.nonEmpty) {
              val delays = edges.map(_.delay)
              val length = delays.max // registers needed
              if (length > 0) { // refactor multiple delay lines by single delay line and multiple nodes on it
                val temps: Seq[VirtualNode[T]] = // virtual nodes working as passthroughs
                  (0 until length).map(i => VirtualNode[T](s"${vertex.name}_${outputPort}_delay${i + 1}", vertex.outWidths(outputPort)))
                // end-to-end connection
                graph.addVertices(temps: _*)
                graph.addEdge(vertex(outputPort), temps(0)(0), 1)
                temps.init.zip(temps.tail).foreach { case (prev, next) => graph.addPath(prev >> 1 >> next) }
                // "move" the edges
                edges.zip(delays).foreach { case (edge, delay) =>
                  if (delay > 0) {
                    // schedules are needed here
                    graph.addEdge(temps(delay - 1)(0), edge.target(edge.inOrder), 0, edge.schedules)
                    graph.removeEdge(edge)
                  }
                }
              }
            }
          }
        case through: PassThrough[_] => // do nothing
        case _ => // do nothing
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

package Chainsaw.DFG

import spinal.core._

class DFGRegOpt[T <: Data](dfg: DFGGraph[T], dataReset: Boolean = false)(implicit val holderProvider: BitCount => T) {

  implicit val currentDFG = dfg

  def getRegMergedDFG: DFGGraph[T] = {
    val optGraph: DFGGraph[T] = dfg.clone().asInstanceOf[DFGGraph[T]]
    //
    optGraph.foreachVertex { vertex =>
      val edgeGroups: Seq[(Int, Seq[DSPEdge[T]])] = vertex.outgoingEdges // for all the outgoing edges from a node
        .groupBy(_.outOrder).toSeq // group by output port, an element is portIndex -> edges from this port
      edgeGroups.foreach { case (outputPort, edges) =>
        if (edges.nonEmpty) {
          val delays = edges.map(_.delay)
          val length = delays.max // registers needed
          if (length > 0) {
            val temps: Seq[VirtualNode[T]] = (0 until length).map(i =>
              VirtualNode[T](s"${vertex.name}.${outputPort}_delay${i + 1}", width = vertex.hardware.outWidths(outputPort)))
            val starts = vertex +: temps.init
            val ends = temps
            starts.zip(ends).foreach { case (start, end) => optGraph.addPath(start >> 1 >> end) }

            edges.zip(delays).foreach { case (edge, delay) =>
              if (delay > 0) {
                optGraph.addPath(temps(delay - 1) >> 0 >> optGraph.getEdgeTarget(edge))
                optGraph.removeEdge(edge)
              }
            }
          }
        }
      }
    }
    optGraph
  }
}

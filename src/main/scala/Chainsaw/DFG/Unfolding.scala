package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

class Unfolding[T <: Data](dfg: DFGGraph[T], unfoldingFactor: Int) {

  val logger: Logger = LoggerFactory.getLogger(classOf[Unfolding[T]])

  // TODO: considering fold a dfg by 2 and then unfold it by 4 - should this be supported?
  // currently, we do not  allow this
  if (!dfg.hasNoParallelEdge) require(dfg.globalLcm % unfoldingFactor == 0) // when there's mux and we use unfolding

  /** Preprocess the dfg to separate delay and mux
   *
   */
  def preprocessed: DFGGraph[T] = {
    val preprocessedDFG = dfg.clone().asInstanceOf[DFGGraph[T]]

    logger.info(s"original DFG:\n$dfg")

    implicit def currentDFG: DFGGraph[T] = preprocessedDFG

    preprocessedDFG.foreachEdge { edge =>
      val source = edge.source
      val target = edge.target
      if (!edge.hasNoMux && edge.weight != 0) {
        val virtualNode = edge.source.extendVirtual(edge.outOrder)
        // separate MUX and delay
        preprocessedDFG.addVertex(virtualNode)
        preprocessedDFG.addEdge(source, virtualNode, edge.outOrder, 0, edge.weight) // the edge with delay
        preprocessedDFG.addEdge(virtualNode(0), target(edge.inOrder), 0, edge.schedules) // the edge with MUX
        preprocessedDFG.removeEdge(edge) // replace the original MUX
      }
    }
    preprocessedDFG
  }

  def unfolded: DFGGraph[T] = {
    implicit val preprocessedDFG: DFGGraph[T] = preprocessed
    val unfoldedDFG = DFGGraph[T](dfg.holderProvider)
    val nodeMap = preprocessedDFG.vertexSeq.map(vertex => vertex -> (0 until unfoldingFactor).map(i => vertex.copy(s"${vertex.name}_unfolded_$i"))).toMap
    preprocessedDFG.foreachEdge { edge =>
      val w = edge.weightWithSource
      val sources = nodeMap(edge.source)
      val targets = nodeMap(edge.target)

      if (edge.hasNoMux) { // for edges with no MUX
        (0 until unfoldingFactor).foreach { i =>
          val j = (i + w) % unfoldingFactor
          val (source, target) = (sources(i), targets(j)) // determining new connection
          unfoldedDFG.addVertex(source)
          unfoldedDFG.addVertex(target)
          val unfoldedDelay = (i + w) / unfoldingFactor // determining new delay
          unfoldedDFG.addEdge(source(edge.outOrder), target(edge.inOrder), unfoldedDelay, NoMUX())
        }
      } else { // for edges with MUX
        require(edge.weight == 0)
        (0 until unfoldingFactor).foreach { i =>
          val (source, target) = (sources(i), targets(i)) // determining new connection
          val unfoldedSchedules = edge.schedules.filter(_.time % unfoldingFactor == i) // MUX transformation
            .map(schedule => Schedule(schedule.time / unfoldingFactor, schedule.period / unfoldingFactor))
          if (unfoldedSchedules.nonEmpty) {
            unfoldedDFG.addVertex(source)
            unfoldedDFG.addVertex(target)
            unfoldedDFG.addEdge(source(edge.outOrder), target(edge.inOrder), 0, unfoldedSchedules)
          }
        }
      }
    }
    unfoldedDFG
  }
}

package Chainsaw.DFG

import org.slf4j.LoggerFactory
import spinal.core._

// TODO: when inner delay = 3, edge delay = 1, N = 2, we need a pre-retiming
class Unfolding[T <: Data](dfg: DFGGraph[T], unfoldingFactor: Int) extends Transform {

  val logger = LoggerFactory.getLogger("unfolding procedure")

  if (!dfg.hasNoParallelEdge) require(dfg.globalLcm % unfoldingFactor == 0) // when there's mux and we use unfolding

  /** Preprocess the dfg to separate delay and mux
   */
  lazy val preprocessed: DFGGraph[T] = {
    implicit val preprocessedDFG = dfg.clone().asInstanceOf[DFGGraph[T]]
    logger.debug(s"original DFG:\n$dfg")
    preprocessedDFG.foreachEdge { edge =>
      val (source, target) = (edge.source, edge.target)
      if (!edge.hasNoMux && edge.weight != 0) { // when there's mux & delay on the same edge
        // separate MUX and delay
        val virtualNode = edge.source.extendVirtual(edge.outOrder)
        preprocessedDFG.addVertex(virtualNode)
        preprocessedDFG.addEdge(source, virtualNode, edge.outOrder, 0, edge.weight) // the edge with delay
        preprocessedDFG.addEdge(virtualNode(0), target(edge.inOrder), 0, edge.schedules) // the edge with MUX
        preprocessedDFG.removeEdge(edge) // replace the original MUX
      }
    }
    logger.debug(s"preprocessedDFG\n$preprocessedDFG")
    preprocessedDFG
  }

  /** Unfolding algo
   */
  lazy val unfolded: DFGGraph[T] = {
    logger.info("start unfolding")
    implicit val preprocessedDFG: DFGGraph[T] = preprocessed
    val unfoldedDFG = DFGGraph[T](s"${dfg.name}_unfolded")
    // nodes duplicated on the unfolded DFG, including I/O nodes
    val nodeMap = preprocessedDFG.vertexSeq.map(vertex => vertex -> (0 until unfoldingFactor).map(i => vertex.copy(s"${vertex.name}_unfolded_$i"))).toMap
    preprocessedDFG.foreachEdge { edge =>
      val (sources, targets) = (nodeMap(edge.source), nodeMap(edge.target))
      val w = edge.weightWithSource

      if (edge.hasNoMux) { // for edges with no MUX
        (0 until unfoldingFactor).foreach { i =>
          val j = (i + w) % unfoldingFactor
          val (source, target) = (sources(i), targets(j)) // determining new connection
          unfoldedDFG.addVertices(source, target)
          val unfoldedDelay = ((i + w) / unfoldingFactor) - source.delay // determining new delay
          unfoldedDFG.addEdge(source(edge.outOrder), target(edge.inOrder), unfoldedDelay, NoMUX())
        }
      } else { // for edges with MUX
        require(edge.weight == 0)
        (0 until unfoldingFactor).foreach { i =>
          val (source, target) = (sources(i), targets(i)) // determining new connection
          // MUX transformation, current time = N(Ml + m) + i => unfolding => Ml + m
          // unfolded time = Nm + i / N = m, unfolded period = current period / N = M
          val unfoldedSchedules = edge.schedules.filter(_.time % unfoldingFactor == i)
            .map(schedule => Schedule(schedule.time / unfoldingFactor, schedule.period / unfoldingFactor))
          if (unfoldedSchedules.nonEmpty) { // or else, this is void after unfolding
            unfoldedDFG.addVertices(source, target)
            unfoldedDFG.addEdge(source(edge.outOrder), target(edge.inOrder), 0, unfoldedSchedules)
          }
        }
      }
    }
    logger.debug(s"unfoldedDFG\n$unfoldedDFG")
    unfoldedDFG
  }

  // TODO: implement 1/N more fluently, considering how this can cascade with other transform
  override def latencyTransformations: Seq[LatencyTrans] = Seq(LatencyTrans(-unfoldingFactor, 0))
}

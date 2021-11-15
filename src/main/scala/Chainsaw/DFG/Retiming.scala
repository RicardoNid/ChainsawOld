package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

/** providing algorithms on retiming DFG
 *
 * @param solution a map of DSPNode -> its retiming value, it doesn't have to contain all the nodes
 */
class Retiming[T <: Data](override val dfg: DFGGraph[T], solution: Map[DSPNode[T], Int]) extends Transform[T] {

  val logger: Logger = LoggerFactory.getLogger("retiming procedure")

  def r(node: DSPNode[T]) = solution.getOrElse(node, 0)

  override lazy val transformed: DFGGraph[T] = {
    logger.info(s"\n\tstart retiming dfg[${dfg.name}]")
    implicit val retimedDFG: DFGGraph[T] = dfg.clone().asInstanceOf[DFGGraph[T]]
    retimedDFG.foreachEdge { edge =>
      // regard nodes not in the solution as 0(static)
      val ru = r(edge.source)
      val rv = r(edge.target)
      // delay transformation: new delay = delay + r(v) - r(u)
      retimedDFG.setEdgeWeight(edge, edge.delay + rv - ru)
      // MUX transformation: new time = (time + r(v) % period)
      retimedDFG.setEdgeSchedules(edge, edge.schedules.map(schedule =>
        Schedule((schedule.time + rv) % schedule.period, schedule.period)))
    }

    //    adjusting ioPosition
    logger.info(s"retiming IO adjustment: ${retimedDFG.ioPositions.mkString(" ")}, according to\n${solution.mkString(" ")}")
    val referenceValue = r(retimedDFG.inputNodes.head)
    retimedDFG.ioNodes.foreach(node => retimedDFG.ioPositions(node) = retimedDFG.ioPositions(node) + r(node) - referenceValue)
    logger.info(s"retiming IO adjustment: ${retimedDFG.ioPositions.mkString(" ")}")

    retimedDFG
  }

  override def latencyTransformations: Seq[LatencyTrans] =
    Seq(LatencyTrans(1, solution(dfg.outputNodes.head) - solution(dfg.inputNodes.head)))

//  logIO()
}

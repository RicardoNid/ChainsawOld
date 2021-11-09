package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

/** providing algorithms on retiming DFG
 *
 * @param solution a map of DSPNode -> its retiming value, it doesn't have to contain all the nodes
 */
class Retiming[T <: Data](val dfg: DFGGraph[T], solution: Map[DSPNode[T], Int]) extends Transform {

  val logger: Logger = LoggerFactory.getLogger("retiming procedure")

  lazy val retimed: DFGGraph[T] = {
    logger.info("s\n\tstart retiming dfg[${dfg.name}]")
    implicit val retimedDFG: DFGGraph[T] = dfg.clone().asInstanceOf[DFGGraph[T]]
    retimedDFG.foreachEdge { edge =>
      // regard nodes not in the solution as 0(static)
      val ru = solution.getOrElse(edge.source, 0)
      val rv = solution.getOrElse(edge.target, 0)
      // delay transformation: new delay = delay + r(v) - r(u)
      retimedDFG.setEdgeWeight(edge, edge.weight + rv - ru)
      // MUX transformation: new time = (time + r(v) % period)
      retimedDFG.setEdgeSchedules(edge, edge.schedules.map(schedule =>
        Schedule((schedule.time + rv) % schedule.period, schedule.period)))
    }
    retimedDFG
  }

  override def latencyTransformations: Seq[LatencyTrans] =
    Seq(LatencyTrans(1, solution(dfg.outputNodes.head) - solution(dfg.inputNodes.head)))
}

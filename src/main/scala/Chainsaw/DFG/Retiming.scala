//package Chainsaw.DFG
//
//import org.slf4j.{Logger, LoggerFactory}
//import spinal.core._
//
//import scala.collection.JavaConversions._
//
///** providing algorithms on retiming DFG
// *
// */
//class Retiming[T <: Data](val dfg: DFGGraph[T]) {
//
//  val logger: Logger = LoggerFactory.getLogger("retiming procedure")
//
//  /** retime a DFG according to retiming values
//   * @param solution a map of DSPNode -> its retiming values, it doesn't have to contain all the nodes
//   */
//  def retimed(solution: Map[DSPNode[T], Int]): DFGGraph[T] = {
//
//    def r(node: DSPNode[T]): Int = solution.getOrElse(node, 0)
//
//    logger.info(s"\n\tstart retiming dfg[${dfg.name}]")
//    implicit val retimedDFG: DFGGraph[T] = dfg.clone().asInstanceOf[DFGGraph[T]]
//    retimedDFG.foreachEdge { edge =>
//      // regard nodes not in the solution as 0(static)
//      val ru = r(edge.source)
//      val rv = r(edge.target)
//      // delay transformation: new delay = delay + r(v) - r(u)
//      retimedDFG.setEdgeWeight(edge, edge.delay + rv - ru)
//      // MUX transformation: new time = (time + r(v) % period)
//      retimedDFG.setEdgeSchedules(edge, edge.schedules.map(schedule =>
//        Schedule((schedule.time + rv) % schedule.period, schedule.period)))
//    }
//
//    //    adjusting ioPosition
//    logger.info(s"retiming IO adjustment: ${retimedDFG.ioPositions.mkString(" ")}, according to\n${solution.mkString(" ")}")
//    val referenceValue = r(retimedDFG.inputNodes.head)
//    retimedDFG.ioNodes.foreach(node => retimedDFG.ioPositions(node) = retimedDFG.ioPositions(node) + r(node) - referenceValue)
//    logger.info(s"retiming IO adjustment: ${retimedDFG.ioPositions.mkString(" ")}")
//
//    retimedDFG
//  }
//
//  /** given changes on the inner delays of DSPNodes, retime a DFG such that its function stays the same
//   * @param incrementMap the changes of inner delays, represented as an increment on the previous one, may be negative
//   */
//  def nodeRetiming(incrementMap:Map[DSPNode[T], Int]): DFGGraph[T] = {
//    val cg = ConstraintGraph[T]()
//
//    // when some of the increments > 0, do retiming to assure the validity of the following adjustment
//    val retimedDFG =  if (incrementMap.values.exists(_ > 0)){
//      implicit val referenceDFG: DFGGraph[T] = dfg
//      incrementMap.foreach { case (u, innerDelay) =>
//        u.targets.foreach { v =>
//          cg.addConstraint(u - v <= -innerDelay) // v - u >= innerDelay
//        }
//      }
//      retimed(cg.getSolution)
//    } else dfg
//
//    incrementMap.foreach { case (u, innerDelayIncrement) =>
//      retimedDFG.outgoingEdgesOf(u).foreach(edge =>
//        retimedDFG.setEdgeWeight(edge, retimedDFG.getEdgeWeight(edge) - innerDelayIncrement)
//      )
//    }
//
//    retimedDFG
//  }
//}

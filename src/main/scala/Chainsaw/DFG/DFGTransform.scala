package Chainsaw.DFG

import Chainsaw._
import org.slf4j.Logger
import spinal.core._

case class Iteration[T <: Data](device: DSPNode[T], time: Int) {

  def delayTo(that: Iteration[T]): Int = that.time - time - device.delay

  def schedule(period: Int): Int = time % period

}

/** definition and execution of time-space transformation
 *
 * @tparam T
 */
trait DFGTransform[T <: Data] {

  implicit val dfg: DFGGraph[T]

  // distinct part(definition of a transform)
  val transformName: String

  val logger: Logger

  def periodTransform(period: Int): Int

  def ioMultiple: Int

  def rangeInvolved: Int

  def iterationsInvolved: Seq[Iteration[T]] = dfg.vertexSeq.flatMap(device => (0 until rangeInvolved).map(i => Iteration(device, i)))

  def spaceTransform(iteration: Iteration[T]): DSPNode[T]

  def timeTransform(iteration: Iteration[T]): Int

  def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T]

  def getTransformed: DFGGraph[T]

  // common part
  def timeSpaceTransform(iteration: Iteration[T]): Iteration[T] = Iteration(spaceTransform(iteration), timeTransform(iteration))

  def pairsInvolved: Seq[(Iteration[T], Iteration[T], DSPEdge[T])] = {
    iterationsInvolved.flatMap { sourceIteration => // concrete pairs
      val (u, uT) = (sourceIteration.device, sourceIteration.time)
      u.outgoingEdges.filter { edge => // filter edges start from this source iteration
        edge.schedules.flatMap(schedule => schedule.timesUnderPeriod(dfg.period))
          .contains((edge.weightWithSource + uT) % dfg.period)
      }.map { edge =>
        val targetIteration = Iteration(edge.target, sourceIteration.time + edge.weightWithSource)
        (sourceIteration, targetIteration, edge)
      }
    } ++ // pairs for latency
      (dfg.latencyEdges.flatMap(edge => (0 until ioMultiple).
        map(i => (Iteration(edge.source, 0), Iteration(edge.target, i + edge.weightWithSource), edge)))
        ++ (1 to ioMultiple).map(i => (Iteration(dfg.ioReference, 0), Iteration(dfg.ioReference, i), LatencyEdge[T]())))
  }

  def constraints: Seq[DSPConstraint[T]] = pairsInvolved.map { case (source, target, _) => constraint(source, target) }

  def isValid: Boolean = pairsInvolved.forall { case (source, target, _) => constraint(source, target).value >= 0 }

  def retimed: DFGGraph[T] = {
    val cg = ConstraintGraph[T](constraints)
    new NewRetiming(dfg, cg.getSolution).transformed
  }

  def transformed: DFGGraph[T] = {

    logger.info("start transformation")

    logger.debug(s"\niterationsInvolved\n\t${iterationsInvolved.mkString(" ")}")
    logger.debug(s"\nbefore $transformName:\n\t$dfg")
    val transformedDFG = DFGGraph[T](s"dfg_$transformName")
    val transformedPeriod = periodTransform(dfg.period)

    // make sure that nodes(especially I/O) is in correct order
    iterationsInvolved.map(spaceTransform).foreach(transformedDFG.addVertex(_))

    pairsInvolved.foreach { case (sourceIteration, targetIteration, edge) =>

      val newSourceIteration = timeSpaceTransform(sourceIteration)
      val newTargetIteration = timeSpaceTransform(targetIteration)

      val (uPrime, vPrime) = (newSourceIteration.device, newTargetIteration.device)
      val (tUPrime, tVPrime) = (newSourceIteration.time, newTargetIteration.time)

      transformedDFG.addVertices(uPrime, vPrime)
      if (edge.isLatencyEdge) {
        if (transformName == "unfolding") println(sourceIteration, targetIteration)
        transformedDFG.addLatencyEdge(uPrime, vPrime, tVPrime - tUPrime - uPrime.delay)
      } else {
        transformedDFG.addEdge(
          source = uPrime(edge.outOrder), target = vPrime(edge.inOrder),
          delay = tVPrime - tUPrime - uPrime.delay,
          schedules = Seq(Schedule(tVPrime % transformedPeriod, transformedPeriod)))
      }
    }

    logger.debug(s"$transformName result:\n$transformedDFG")
    val ret = if(! this.isInstanceOf[NewRetiming[T]]) NewRetiming.alignIO(transformedDFG) else transformedDFG
    logger.debug(s"latency after IO alignment:\n${ret.latencyEdges.map(ret.getEdgeWeight(_)).mkString(" ")}")
    ret
  }
}



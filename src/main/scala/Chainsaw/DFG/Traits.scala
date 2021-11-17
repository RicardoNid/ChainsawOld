package Chainsaw.DFG

import Chainsaw._
import org.slf4j.Logger
import spinal.core._

case class Iteration[T <: Data](device: DSPNode[T], time: Int)

/** definition and execution of time-space transformation
 *
 * @tparam T
 */
trait Transform[T <: Data] {

  implicit val dfg: DFGGraph[T]

  val transformName: String

  def timeSpaceTransform(iteration: Iteration[T]): Iteration[T]

  def periodTransform(period: Int): Int

  def iterationsInvolved: Seq[Iteration[T]]

  def pairsInvolved: Seq[(Iteration[T], Iteration[T], DSPEdge[T])] = {
    iterationsInvolved.flatMap { sourceIteration =>
      val (u, uT) = (sourceIteration.device, sourceIteration.time)
      u.outgoingEdges.filter { edge => // filter edges start from this source iteration
        edge.schedules.flatMap(schedule => schedule.timesUnderPeriod(dfg.period))
          .contains((edge.weightWithSource + uT) % dfg.period)
      }.map { edge =>
        val targetIteration = Iteration(edge.target, sourceIteration.time + edge.weightWithSource)
        (sourceIteration, targetIteration, edge)
      }
    } ++
      dfg.latencyEdges.map(edge => (Iteration(edge.source, 0), Iteration(edge.target, edge.weightWithSource), edge))
  }

  def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T]

  def constraints: Seq[DSPConstraint[T]] = pairsInvolved.map { case (source, target, _) => constraint(source, target) }

  def isValid: Boolean = pairsInvolved.forall { case (source, target, _) => constraint(source, target).value >= 0 }

  def retimed: DFGGraph[T] = {
    val cg = ConstraintGraph[T](constraints)
    new NewRetiming(dfg, cg.getSolution).build
  }

  def build: DFGGraph[T] = {

    logger.info(s"iterationsInvolved\n${iterationsInvolved.mkString(" ")}")

    logger.info(s"before $transformName:\n$dfg")
    val transformedDFG = DFGGraph[T](s"dfg_$transformName")
    val transformedPeriod = periodTransform(dfg.period)

    // make sure that nodes(especially I/O) is in correct order
    iterationsInvolved.map(timeSpaceTransform).map(_.device).foreach(transformedDFG.addVertex(_))

    pairsInvolved.foreach { case (sourceIteration, targetIteration, edge) =>

      val newSourceIteration = timeSpaceTransform(sourceIteration)
      val newTargetIteration = timeSpaceTransform(targetIteration)

      val (uPrime, vPrime) = (newSourceIteration.device, newTargetIteration.device)
      val (tUPrime, tVPrime) = (newSourceIteration.time, newTargetIteration.time)

      transformedDFG.addVertices(uPrime, vPrime)
      if (edge.isLatencyEdge) {
        transformedDFG.addLatencyEdge(uPrime, vPrime, tVPrime - tUPrime - uPrime.delay)
      } else {
        transformedDFG.addEdge(
          source = uPrime(edge.outOrder), target = vPrime(edge.inOrder),
          delay = tVPrime - tUPrime - uPrime.delay,
          schedules = Seq(Schedule(tVPrime % transformedPeriod, transformedPeriod)))
      }
    }

    logger.info(s"$transformName result:\n$transformedDFG")
    val ret = if (transformName == "folding") NewRetiming.alignIO(transformedDFG) else transformedDFG
    //    val ret = transformedDFG
    logger.info(s"latency after IO alignment:\n${ret.latencyEdges.map(ret.getEdgeWeight(_)).mkString(" ")}")
    //    logger.info(s"latency after IO alignment:\n${ret.latency}")
    ret
  }
}

trait DFGGen[T <: Data] {
  def getGraph: DFGGraph[T]

  def latency: Int

  def getGraphAsNode(dataReset: Boolean = false)(implicit holderProvider: HolderProvider[T]): DSPNode[T]
}

trait NodeComponent[T <: Data] {
  val dataIn: Vec[T]
  val dataOut: Vec[T]
}

package Chainsaw.DFG

import Chainsaw._
import org.slf4j.Logger
import spinal.core._

case class Iteration[T <: Data](device: DSPNode[T], time: Int)

trait Transform[T <: Data] {

  implicit val dfg: DFGGraph[T]

  val transformName: String

  def timeSpaceTransform(iteration: Iteration[T]): Iteration[T]

  def periodTransform(period: Int): Int

  def iterationsInvolved: Seq[Iteration[T]]

  def build: DFGGraph[T] = {

    logger.info(s"iterationsInvolved\n${iterationsInvolved.mkString(" ")}")

    logger.info(s"before $transformName:\n$dfg")
    val transformedDFG = DFGGraph[T](s"dfg_$transformName")
    val transformedPeriod = periodTransform(dfg.period)

    iterationsInvolved.foreach { sourceIteration =>
      val (u, uT) = (sourceIteration.device, sourceIteration.time)
      u.outgoingEdges
        .filter { edge =>
          edge.schedules.flatMap(schedule => schedule.timesUnderPeriod(dfg.period))
            .contains((edge.weightWithSource + uT) % dfg.period)
        }
        .foreach { edge =>
          val targetIteration = Iteration(edge.target, sourceIteration.time + edge.weightWithSource)

          val newSourceIteration = timeSpaceTransform(sourceIteration)
          val newTargetIteration = timeSpaceTransform(targetIteration)

          val (uPrime, vPrime) = (newSourceIteration.device, newTargetIteration.device)
          val (tUPrime, tVPrime) = (newSourceIteration.time, newTargetIteration.time)

          transformedDFG.addVertices(uPrime, vPrime)
          transformedDFG.addEdge(
            source = uPrime(edge.outOrder), target = vPrime(edge.inOrder),
            delay = tVPrime - tUPrime - uPrime.delay,
            schedules = Seq(Schedule(tVPrime % transformedPeriod, transformedPeriod)))
        }
    }

    logger.info(s"$transformName result:\n$transformedDFG")
    transformedDFG
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

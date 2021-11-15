package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core.Data
import scala.math.floor

/** providing algorithms on folding DFG
 *
 * @param foldingSet 2D folding set which contains Seq of Seq of nodes, null nodes may be included
 */
class Folding[T <: Data](override val dfg: DFGGraph[T], foldingSet: Seq[Seq[DSPNode[T]]]) extends Transform[T] {
  val logger: Logger = LoggerFactory.getLogger("folding procedure")

  // preparing context for folding
  val N: Int = foldingSet.head.length // folding factor
  require(foldingSet.forall(_.size == N), "folding set should have the same folding factor on each node")

  // step1: preparing the environment of folding algorithm
  val foldingOrderOf: Map[DSPNode[T], Int] = foldingSet.flatMap { set => // map node -> folding order of the node
    set.zipWithIndex.filterNot(_._1 == null) map { case (node, i) => node -> i }
  }.toMap

  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSet.flatMap { nodes => // map node -> folded device of the node
    val nonEmptyNodes = nodes.filterNot(_ == null)
    nonEmptyNodes.map(_ -> nonEmptyNodes.head)
  }.toMap

  val devices: Seq[DSPNode[T]] = deviceOf.values.toSeq.distinct

  // step2: do retiming such that the DFG is legal to be folded
  lazy val retimingSolution: Map[DSPNode[T], Int] = {
    implicit val sourceDFG: DFGGraph[T] = dfg
    val cg = ConstraintGraph(dfg)
    logger.debug(s"original folded delays ${dfg.edgeSeq.map(getDelay).mkString(" ")}")
    dfg.foreachEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val U = edge.source
      val V = edge.target
      // folded delay >= 0 => N * (r(V) - r(U) + w) - Pu + v - u >= 0 => r(U) - r(V) <= (Nw + - Pu + v - u)/N
      cg.addConstraint(U - V <= floor(getDelay(edge).toDouble / N).toInt)
    }
    cg.getSolution
  }

  // the procedure of constructing folded DFG
  override lazy val transformed: DFGGraph[T] = {

    logger.info(s"\n\tstart folding dfg[${dfg.name}]")
    logger.debug(s"original:\n$dfg")

    // step2: do retiming such that the DFG is legal to be folded
    val solution = retimingSolution
    // the retimed graph is the reference DFG of following transformations
    implicit val retimedDFG: DFGGraph[T] = dfg.retimed(solution)

    logger.debug(s"\n\tretiming solution:\n${solution.mkString(" ")}" + // log on retiming stage
      s"retimed dfg:\n$retimedDFG" +
      s"\n\tretimed folded delays ${retimedDFG.edgeSeq.map(getDelay).mkString(" ")}")

    // step3: adding nodes
    val foldedDFG = DFGGraph[T](s"${dfg.name}_folded")
    foldedDFG.addVertices(retimedDFG.outerNodes: _*) // keep outer nodes the same
    foldedDFG.addVertices(devices: _*) // fold inner nodes(device nodes) into devices
    foldedDFG.ioPositions ++= retimedDFG.ioPositions

    // step4: adding edges
    retimedDFG.foreachEdge { edge =>
      val V = edge.target
      val U = edge.source
      val Hu = if (U.isOuter) U else deviceOf(U)
      val Hv = if (V.isOuter) V else deviceOf(V)
      val (foldedDelay, foldedSchedules) = getTransformed(edge)
      foldedDFG.addEdge(Hu(edge.outOrder), Hv(edge.inOrder), foldedDelay, foldedSchedules)
    }
    logger.debug(s"folded dfg:\n$foldedDFG")

    // adjusting ioPosition
    val before = foldedDFG.ioPositions.toSeq
    foldedDFG.inputNodes.foreach(node => foldedDFG.ioPositions(node) = foldedDFG.ioPositions(node) * N)
    foldedDFG.outputNodes.foreach(node => foldedDFG.ioPositions(node) = foldedDFG.ioPositions(node) * N + foldingOrderOf(node.sources.head))
    val after = foldedDFG.ioPositions.toSeq
    logger.info(s"folding IO adjustment: ${before.diff(after).mkString(" ")} -> ${after.diff(before).mkString(" ")} ")

    // TODO: align the outputs ASAP
    // step5: align the output schedule
    val ret = alignOutput(foldedDFG)
    ret

  }

  /** calculator transforming delay -> folded delay and schedules -> folded schedules
   *
   * @param edge         an edge in the source dfg
   * @param referenceDFG the source dfg
   * @see we follow the notations and formality in
   *
   *      ''VLSI Digital Signal Processing Systems: Design and Implementation'', 1999
   *      and
   *
   *      ''Synthesis of Control Circuits in Folded Pipelined DSP Architectures'', 1992
   */
  def getTransformed(edge: DSPEdge[T])(implicit referenceDFG: DFGGraph[T]): (Int, Seq[Schedule]) = {
    val U = edge.source
    val V = edge.target
    val u = if (U.isOuter) foldingOrderOf(V) else foldingOrderOf(U) // the folding order of an input follows its target
    val v = if (V.isOuter) foldingOrderOf(U) else foldingOrderOf(V) // the folding order of an output follows its source
    val w = edge.weightWithSource
    val Pu = if (U.isOuter) 0 else deviceOf(U).delay

    val newSchedules = edge.schedules.map { schedule => // new delay
      val newPeriod = schedule.period * N
      logger.debug(s"calculating schedule time: ${schedule.time} * $N * $v")
      val newTime = (schedule.time * N + v) % newPeriod
      Schedule(newTime, newPeriod)
    }

    logger.debug(s"calculating delay: $N * $w - $Pu + $v - $u = ${N * w - Pu + v - u}")
    val newDelay = N * w - Pu + v - u

    (newDelay, newSchedules)
  }

  def getDelay(edge: DSPEdge[T])(implicit referenceDFG: DFGGraph[T]): Int = getTransformed(edge)._1

  def alignOutput(foldedDFG: DFGGraph[T]): DFGGraph[T] = {

    // N is the period, and time % N is the position
    // % N because the original DFG may have MUX, and thus, the "global period" is not the same as the local one
    implicit val referenceDFG: DFGGraph[T] = foldedDFG

    val ref = foldedDFG.inputPositions.min
    val inputRetimingSolution = foldedDFG.inputNodes.map { input =>
      if(foldedDFG.ioPositions(input) >= N + ref)  input -> (ref - foldedDFG.ioPositions(input))// extra delay needed
      else input -> 0
    }.toMap

    println(s"input position ${foldedDFG.inputPositions.mkString(" ")}")

    val inputRetimed = foldedDFG.retimed(inputRetimingSolution)

    println(s"input position ${inputRetimed.inputPositions.mkString(" ")}")

    val outputRetimingSolution = inputRetimed.outputNodes.map { output =>
      output -> (inputRetimed.outputPositions.max - inputRetimed.ioPositions(output)) // extra delay needed
    }.toMap

    val ret = inputRetimed.retimed(outputRetimingSolution)

    logger.debug(s"retiming output of folded dfg:\n${outputRetimingSolution.mkString(" ")}")

    logger.info(
      s"\n\t${foldedDFG.delayAmount} buffer regs in the folded DFG" +
        s"\n\t${outputRetimingSolution.values.sum} output regs in the folded DFG" +
        s"\n\t${ret.delayAmount} regs in total"
    )
    ret
  }

  override def latencyTransformations: Seq[LatencyTrans] = {
    implicit val foldedDFG: DFGGraph[T] = transformed // TODO: avoid rerun this
    val inputSchedule: Int = foldedDFG.inputNodes.head.outgoingEdges.flatMap(_.schedules.map(_.time)).min
    val outputSchedule: Int = foldedDFG.outputNodes.head.incomingEdges.head.schedules.head.time
    new Retiming(dfg, retimingSolution).latencyTransformations :+ LatencyTrans(N, outputSchedule - inputSchedule)
  }
}

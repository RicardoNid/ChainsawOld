package Chainsaw.DFG

import Chainsaw._
import jdk.jfr.Experimental
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/** providing algorithms on implementing DFG as hardware component
  *
  * @param referenceDFG
  *   dfg to be implemented as hardware
  * @param useRegInit
  *   when set, registers on the edge are zero-initialized
  * @param holderProvider
  *   method that generate a signal of a given width
  * @see
  *   [[DFG]] package object for default holder providers of DFG
  * @see
  *   [[examples.MultiModuleExample]] for submodule sharing mechanism
  */
@Experimental // the "submodule" mode is experimental, it improves the gen result, but may lead to width inference problem
class DFGImpl[T <: Data](dfg: DFGGraph[T], useRegInit: Boolean = true, useSubmodule: Boolean = true)(implicit val holderProvider: HolderProvider[T]) {

  val logger: Logger = LoggerFactory.getLogger(s"implementing procedure")

  // do regs merging before
  implicit val preprocessed: DFGGraph[T] = if (!dfg.isMerged) dfg.merged else dfg

  logger.info(
    s"\n\tstart implementing DFG[${dfg.name}], with config: " +
      s"\n\t\tregInit = ${globalImplPolicy.useRegInit}, useSubmodule = ${globalImplPolicy.useSubmodule}" +
      s"\n\tnumber of nodes: ${dfg.innerNodes.size}, number of modules: ${dfg.innerNodes.map(_.hardware).distinct.size}" +
      s"\n\tmerging regs on edges: ${dfg.unmergedDelayAmount} -> ${preprocessed.unmergedDelayAmount}" +
      s"\n\talignment regs amount: ${preprocessed.alignmentDelayAmount}"
  )

  // attributes tobe used, they provide the "environment" of "synthesis"
  val isRecursive: Boolean                           = preprocessed.isRecursive
  val vertexSeq: Seq[DSPNode[T]]                     = preprocessed.vertexSeq
  val period: Int                                    = preprocessed.period
  val inputNodes: Seq[DSPNode[T]]                    = preprocessed.inputNodes
  val outputNodes: Seq[DSPNode[T]]                   = preprocessed.outputNodes
  val signalMap: mutable.Map[DSPNode[T], Seq[T]]     = mutable.Map[DSPNode[T], Seq[T]]() // storing the output signals of implemented nodes
  val delayMap: mutable.Map[DSPNode[T], Seq[Vec[T]]] = mutable.Map[DSPNode[T], Seq[Vec[T]]]()

  val hardwareGroups: Map[DSPHardware[T], Seq[DSPNode[T]]] = dfg.innerNodes.groupBy(_.hardware) // nodes grouped by different hardware
  // a map from DSPNode to its hardware component, making multiple nodes share a same component
  val componentMap: Map[DSPNode[T], () => Component with NodeComponent[T]] = hardwareGroups
    .map { case (hardware, nodes) =>
      val commonName: String = nodes.map(_.name).reduce(_ intersect _).filter(_.isLetterOrDigit)
      val component          = hardware.asComponent(commonName)
      nodes.map(node => node -> component)
    }
    .flatten
    .toMap

  def nameSignal(signals: Seq[T], target: DSPNode[T]): Unit = {
    if (signals.size == 1) signals.head.setName(target.name, weak = true)
    else signals.zipWithIndex.foreach { case (signal, i) => signal.setName(s"${target}_$i", weak = true) }
  }

  def initGlobalCount(): GlobalCount = { //
    val globalCounter = CounterFreeRun(period)
    globalCounter.value.setName("globalCounter")
    GlobalCount(globalCounter.value)
  }

  /** implement a vertex by its hardware and all its driving signals
    *
    * @param target
    *   the vertex to
    * @param signalMap
    *   map of DSPNode -> its output signals
    * @param globalCount
    *   the global counter value in this DFG
    * @return
    *   output signals of the node just implemented
    */
  def implVertex(target: DSPNode[T], signalMap: mutable.Map[DSPNode[T], Seq[T]])(implicit globalCount: GlobalCount): Seq[T] = {

    // step1: constructing all driving signals(dataIns)
    val dataGroups: Seq[(Int, Seq[DSPEdge[T]])] = target.incomingEdges // group all incoming edges by input port
      .groupBy(_.inOrder)
      .toSeq
      .sortBy(_._1) // sort by the order so that "dataInsOnPorts" is in order

    val dataInsOnPorts: Seq[T] = // implement dataIns one port at a time

      dataGroups.map { case (portNumber, singlePortEdges) => // combine dataIns at the same port by a mux
        val isROM = singlePortEdges.forall(_.source.isConstant)
        val singlePortData: Seq[T] = {
          if (isROM) singlePortEdges.map(_.source.asInstanceOf[ConstantNode[T]].getConstant)
          else
            singlePortEdges.map { edge => // gets the delayed version of data
              val dataIn = signalMap(edge.source)(edge.outOrder) // get the data at driver's output port
              if (globalImplPolicy.useRegInit) Delay(dataIn, edge.weight.toInt, init = dataIn.getZero) // get the delayed version
              else Delay(dataIn, edge.weight.toInt)
            }
        }

        // implement the MUX
        val schedulesOnePort: Seq[Seq[Schedule]] = singlePortEdges.map(_.schedules) // get the schedules
        val succeed                              = Try(DFGMUX[T](schedulesOnePort, period).impl(singlePortData, asROM = false)) // data + schedules -> MUX
        succeed match {
          case Failure(exception) =>
            throw new IllegalArgumentException(
              s"MUX impl failed on:\n${singlePortEdges.map(edge => s"${edge.symbol} ${edge.schedules.mkString(" ")}").mkString("\n")}"
            )
          case Success(value) => value
        }
      }

    // step2: driving the node and get the output
    // option: using shared submodule/anno function
    if (
      globalImplPolicy.useSubmodule &&
      !target.isInstanceOf[VirtualNode[T]] && componentMap.contains(target)
      && !isRecursive
    ) { // when available, implement it by submodule
      logger.info(s"implementing $target using submodule, inputWidths = ${dataInsOnPorts.map(_.getBitsWidth).mkString(" ")}")
      val nodeComponent = componentMap(target)()
      nodeComponent.dataIn := Vec(dataInsOnPorts)
      nodeComponent.dataOut
    } // else, by anno function
    else target.impl(dataInsOnPorts, globalCount)
  }

  // TODO: consider the width inference in recursive DFG seriously

  def impl: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    logger.info(s"implementing DFG by algo for ${if (isRecursive) "recursive" else "forwarding"} DFG")
    // check validity
    require(inputNodes.size == dataIns.size, "input size mismatch")

    // step1: initialize the global counter
    implicit val globalCount: GlobalCount = initGlobalCount()

    // step2: initialize the signalMap
    if (isRecursive) { // insert placeholders into the signalMap
      vertexSeq.foreach { node =>
        if (node.outWidths.exists(_.value == -1) && node.isInner) // nodes with undetermined width can be dangerous in recursive DFG
          logger.warn(s"node $node has undetermined width ${node.outWidths.mkString(" ")} in a recursive DFG")
        signalMap += (node -> node.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i)))
      }
      // implement inputs, for recursive, we "connect", for forwarding, we "create"
      inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }
    }
    // implement inputs
    else inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap += node -> Seq(bits) }

    // step3: implement other nodes one by one
    if (isRecursive) { // for recursive DFG, as we use placeholders, we can implement nodes in any order(when widths given)
      vertexSeq.diff(inputNodes).foreach { target =>
        val rets         = implVertex(target, signalMap) // invoke implVertex to implement a node
        val placeholders = signalMap(target)
        placeholders.zip(rets).foreach { case (placeholder, ret) => placeholder := ret.resized }
        nameSignal(placeholders, target)
      }
    } else { // for forwarding DFG, we can only implement nodes whose drivers have already been implemented
      // nodes already implemented
      def implemented: Seq[DSPNode[T]] = signalMap.keys.toSeq

      // nodes not implemented yet
      def remained: Seq[DSPNode[T]] = vertexSeq.diff(implemented)

      // nodes ready to be implemented
      def nextStageNodes: Seq[DSPNode[T]] = remained.filter(_.sources.forall(implemented.contains(_)))

      // until all nodes
      while (nextStageNodes.nonEmpty) {
        nextStageNodes.foreach { target =>
          val rets: Seq[T] = implVertex(target, signalMap)
          signalMap += target -> rets
          nameSignal(rets, target)
        }
      }
      if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")
    }

    // step4 : return the signals at output ports as result
    outputNodes.flatMap(signalMap(_))
  }
}

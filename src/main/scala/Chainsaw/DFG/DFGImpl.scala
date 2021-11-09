package Chainsaw.DFG

import Chainsaw._
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/** providing algorithms on implementing DFG as hardware component
 *
 * @param dfg            dfg to be implemented as hardware
 * @param dataReset      when set, registers on the edge are zero-initialized
 * @param holderProvider method that generate a signal of a given width
 * @see [[DFG]] package object for default holder providers of DFG
 * @see [[examples.MultiModuleExample]] for submodule sharing mechanism
 */
class DFGImpl[T <: Data](dfg: DFGGraph[T], dataReset: Boolean = true)
                        (implicit val holderProvider: HolderProvider[T]) {

  val logger: Logger = LoggerFactory.getLogger(s"implementing procedure")
  logger.info(s"implementing dfg ${dfg.name}, with dataReset = $dataReset")
  logger.info(s"number of nodes: ${dfg.innerNodes.size}, number of modules: ${dfg.innerNodes.map(_.hardware).distinct.size}")

  implicit def referenceDFG: DFGGraph[T] = dfg

  // attributes tobe used, they provide the "environment" of "synthesis"
  val isRecursive: Boolean = dfg.isRecursive
  val vertexSeq: Seq[DSPNode[T]] = dfg.vertexSeq
  val globalLcm: Int = dfg.globalLcm
  val inputNodes: Seq[DSPNode[T]] = dfg.inputNodes
  val outputNodes: Seq[DSPNode[T]] = dfg.outputNodes
  val signalMap: mutable.Map[DSPNode[T], Seq[T]] = mutable.Map[DSPNode[T], Seq[T]]() // storing the output signals of implemented nodes
  val delayMap: mutable.Map[DSPNode[T], Seq[Vec[T]]] = mutable.Map[DSPNode[T], Seq[Vec[T]]]()

  val hardware2component: Map[DSPHardware[T], () => Component with NodeComponent[T]] = // a map from DSPNode to its hardware component
    dfg.innerNodes.map(_.hardware).distinct // nodes who have same hardware implementation share submodule
      .map(hardware => hardware -> hardware.asComponent).toMap
  val componentMap: Map[DSPNode[T], () => Component with NodeComponent[T]] = dfg.innerNodes.map(node => node -> hardware2component(node.hardware)).toMap

  def nameSignal(signals: Seq[T], target: DSPNode[T]): Unit = {
    if (signals.size == 1) signals.head.setName(target.name, weak = true)
    else signals.zipWithIndex.foreach { case (signal, i) => signal.setName(s"${target}_$i", weak = true) }
  }

  def initGlobalCount(): GlobalCount = { //
    val globalCounter = CounterFreeRun(globalLcm)
    globalCounter.value.setName("globalCounter", weak = true)
    GlobalCount(globalCounter.value)
  }

  /** implement a vertex by its hardware and all its driving signals
   *
   * @param target      the vertex to
   * @param signalMap   map of DSPNode -> its output signals
   * @param globalCount the global counter value in this DFG
   * @return output signals of the node just implemented
   */
  def implVertex(target: DSPNode[T], signalMap: mutable.Map[DSPNode[T], Seq[T]], delayMap: mutable.Map[DSPNode[T], Seq[Vec[T]]])(implicit globalCount: GlobalCount): Seq[T] = {

    // step1: constructing all driving signals(dataIns)
    val dataGroups: Seq[(Int, Seq[DSPEdge[T]])] = target.incomingEdges // group all incoming edges by input port
      .groupBy(_.inOrder).toSeq
      .sortBy(_._1) // sort by the order so that "dataInsOnPorts" is in order

    val dataInsOnPorts: Seq[T] = // implement dataIns one port at a time

      dataGroups.map { case (portNumber, singlePortEdges) => // combine dataIns at the same port by a mux
        val singlePortData: Seq[T] = singlePortEdges.map { edge => // gets the delayed version of data

          // option: do register merging or not
          //          if (dfg.isForwarding) {
          //            if (edge.delay == 0) signalMap(edge.source)(edge.outOrder)
          //            else delayMap(edge.source)(edge.outOrder)(edge.delay)
          //          }
          //          else {
          //            val dataIn = signalMap(edge.source)(edge.outOrder)
          //            if (dataReset) Delay(dataIn, edge.weight.toInt, init = dataIn.getZero)
          //            else Delay(dataIn, edge.weight.toInt)
          //          }

          val dataIn = signalMap(edge.source)(edge.outOrder) // get the data at driver's output port
          if (dataReset) Delay(dataIn, edge.weight.toInt, init = dataIn.getZero) // get the delayed version
          else Delay(dataIn, edge.weight.toInt)
        }

        // implement the MUX
        val schedulesOnePort: Seq[Seq[Schedule]] = singlePortEdges.map(_.schedules) // get the schedules
        val succeed = Try(DFGMUX[T](schedulesOnePort).impl(singlePortData, globalLcm)) // data + schedules -> MUX
        succeed match {
          case Failure(exception) => throw new IllegalArgumentException(s"MUX impl failed on:\n${singlePortEdges.map(_.symbol).mkString("|")}")
          case Success(value) => value
        }
      }

    // option: using shared submodule/anno function
    //    if (target.isInner && dfg.isForwarding) {
    //      logger.debug(s"implementing $target using submodule, inputWidths = ${dataInsOnPorts.map(_.getBitsWidth).mkString(" ")}")
    //      val nodeComponent = componentMap(target)()
    //      nodeComponent.dataIn := Vec(dataInsOnPorts)
    //      nodeComponent.dataOut
    //    }
    //    else target.hardware.impl(dataInsOnPorts, globalCount)

    // step2: driving the node and get the output
    target.hardware.impl(dataInsOnPorts, globalCount)
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
        if (node.hardware.outWidths.exists(_.value == -1) && node.isInner) // nodes with undetermined width can be dangerous in recursive DFG
          logger.warn(s"node $node has undetermined width ${node.hardware.outWidths.mkString(" ")} in a recursive DFG")
        signalMap += (node -> node.hardware.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i)))
      }
      // implement inputs, for recursive, we "connect", for forwarding, we "create"
      inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }
    }
    // implement inputs
    else inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap += node -> Seq(bits) }

    // step3: implement other nodes one by one
    if (isRecursive) { // for recursive DFG, as we use placeholders, we can implement nodes in any order(when widths given)
      vertexSeq.diff(inputNodes).foreach { target =>
        val rets = implVertex(target, signalMap, delayMap) // invoke implVertex to implement a node
        val placeholders = signalMap(target)
        placeholders.zip(rets).foreach { case (placeholder, ret) => placeholder := ret.resized }
        nameSignal(placeholders, target)
      }
    }
    else { // for forwarding DFG, we can only implement nodes whose drivers have already been implemented
      // nodes already implemented
      def implemented: Seq[DSPNode[T]] = signalMap.keys.toSeq

      // nodes not implemented yet
      def remained: Seq[DSPNode[T]] = vertexSeq.diff(implemented)

      // nodes ready to be implemented
      def nextStageNodes: Seq[DSPNode[T]] = remained.filter(_.sources.forall(implemented.contains(_)))

      // until all nodes
      while (nextStageNodes.nonEmpty) {
        nextStageNodes.foreach { target =>

          val rets: Seq[T] = implVertex(target, signalMap, delayMap)
          signalMap += target -> rets

          //          logger.debug(s"delays ${delayMap.mkString(" ")}")
          //          delayMap += target -> rets.zipWithIndex.map { case (signal, outOrder) =>
          //            val edges = target.outgoingEdges.filter(_.outOrder == outOrder)
          //            if (edges.nonEmpty) {
          //              val delayMax = edges.map(_.delay).max
          //              if (dataReset) History(signal, 0 to delayMax, init = signal.getZero) else History(signal, 0 to delayMax)
          //            } else {
          //              Vec(holderProvider(-1 bits))
          //            }
          //          }

          nameSignal(rets, target)
        }
      }
      if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")
    }

    // step4 : return the signals at output ports as result
    outputNodes.flatMap(signalMap(_))
  }
}

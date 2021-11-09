package Chainsaw.DFG

import Chainsaw._
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
 * @param dfg
 * @param dataReset
 * @param holderProvider
 * @see [[examples.MultiModuleExample]]
 */
class DFGImpl[T <: Data](dfg: DFGGraph[T], dataReset: Boolean = true)
                        (implicit val holderProvider: HolderProvider[T]) {

  val logger: Logger = LoggerFactory.getLogger(s"implementing procedure")
  logger.info(s"implementing dfg ${dfg.name}, dataReset = $dataReset")
  logger.info(s"number of nodes: ${dfg.innerNodes.size}, number of modules: ${dfg.innerNodes.map(_.hardware).distinct.size}")

  implicit def referenceDFG: DFGGraph[T] = dfg

  // attributes tobe used
  val vertexSeq: Seq[DSPNode[T]] = dfg.vertexSeq
  val globalLcm: Int = dfg.globalLcm
  val inputNodes: Seq[DSPNode[T]] = dfg.inputNodes
  val outputNodes: Seq[DSPNode[T]] = dfg.outputNodes

  val hardware2component: Map[DSPHardware[T], () => Component with NodeComponent[T]] = // a map from DSPNode to its hardware component
    dfg.innerNodes.map(_.hardware).distinct // nodes who have same hardware implementation share submodule
      .map(hardware => hardware -> hardware.asComponent).toMap
  val componentMap: Map[DSPNode[T], () => Component with NodeComponent[T]] = dfg.innerNodes.map(node => node -> hardware2component(node.hardware)).toMap

  def initGlobalCount(): GlobalCount = {
    val globalCounter = CounterFreeRun(globalLcm)
    globalCounter.value.setName("globalCounter", weak = true)
    GlobalCount(globalCounter.value)
  }

  def implVertex(target: DSPNode[T], signalMap: mutable.Map[DSPNode[T], Seq[T]], delayMap: mutable.Map[DSPNode[T], Seq[Vec[T]]])(implicit globalCount: GlobalCount): Seq[T] = {

    val dataIns: Seq[(Int, Seq[DSPEdge[T]])] = target.incomingEdges // group all incoming edges by input port
      .groupBy(_.inOrder).toSeq
      .sortBy(_._1) // sort by the order so that "dataInsOnPorts" is in order

    val dataInsOnPorts: Seq[T] = // implement dataIns on different ports
      dataIns.map { case (portNumber, edgesOnePort) => // combine dataIns at the same port by a mux
        val dataCandidates: Seq[T] = edgesOnePort.map { edge => // take the delayed version of these dataIns

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

          val dataIn = signalMap(edge.source)(edge.outOrder)
          if (dataReset) Delay(dataIn, edge.weight.toInt, init = dataIn.getZero)
          else Delay(dataIn, edge.weight.toInt)
        }
        // implement the MUX
        val schedulesOnePort: Seq[Seq[Schedule]] = edgesOnePort.map(_.schedules)
        val mux = DFGMUX[T](schedulesOnePort)
        val succeed = Try(mux.impl(dataCandidates, globalLcm))
        succeed match {
          case Failure(exception) => logger.error(s"MUX impl failed on:\n${edgesOnePort.map(_.symbol).mkString("|")}")
            mux.impl(dataCandidates, globalLcm)
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

    target.hardware.impl(dataInsOnPorts, globalCount)
  }

  // implement a recursive graph
  def implRecursive: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    logger.info("implementing DFG by algo for recursive DFG")
    require(inputNodes.size == dataIns.size, "input size mismatch")

    val signalMap: mutable.Map[DSPNode[T], Seq[T]] = mutable.Map(vertexSeq.map { node => // a map to connect nodes with their outputs(placeholder)
      if (node.hardware.outWidths.exists(_.value == -1)) logger.warn(s"node $node has undetermined width ${node.hardware.outWidths.mkString(" ")} in a recursive DFG")
      node -> node.hardware.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i))
    }: _*)

    val delayMap = mutable.Map[DSPNode[T], Seq[Vec[T]]]()

    implicit val globalCount: GlobalCount = initGlobalCount()

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }
    vertexSeq.diff(inputNodes).foreach { target =>
      val rets = implVertex(target, signalMap, delayMap)
      val placeholders = signalMap(target)
      placeholders.zip(rets).foreach { case (placeholder, ret) => placeholder := ret.resized }
      if (placeholders.size == 1) placeholders.head.setName(target.name, weak = true) // name these signals
      else placeholders.zipWithIndex.foreach { case (placeholder, i) => placeholder.setName(s"${target}_$i", weak = true) }
    }
    outputNodes.flatMap(signalMap(_))
  }

  def implForwarding: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    logger.info("implementing DFG by algo for forwarding DFG")
    require(inputNodes.size == dataIns.size, "input size mismatch")
    val signalMap = mutable.Map[DSPNode[T], Seq[T]]()
    val delayMap = mutable.Map[DSPNode[T], Seq[Vec[T]]]()
    implicit val globalCount: GlobalCount = initGlobalCount()
    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap += node -> Seq(bits) }

    def implemented = signalMap.keys.toSeq

    def remained = vertexSeq.diff(implemented)

    def nextStageNodes = remained.filter(_.sources.forall(implemented.contains(_)))

    while (nextStageNodes.nonEmpty) {
      nextStageNodes.foreach { target =>

        logger.debug(s"delays ${delayMap.mkString(" ")}")

        val rets: Seq[T] = implVertex(target, signalMap, delayMap)
        signalMap += target -> rets

        delayMap += target -> rets.zipWithIndex.map { case (signal, outOrder) =>
          val edges = target.outgoingEdges.filter(_.outOrder == outOrder)
          if (edges.nonEmpty) {
            val delayMax = edges.map(_.delay).max
            if (dataReset) History(signal, 0 to delayMax, init = signal.getZero) else History(signal, 0 to delayMax)
          } else {
            Vec(holderProvider(-1 bits))
          }
        }

        if (target.hardware.outWidths.size == 1) rets.head.setName(target.name, weak = true)
        else rets.zipWithIndex.foreach { case (ret, i) => ret.setName(s"${target}_$i", weak = true) }
      }
    }
    outputNodes.flatMap(signalMap(_))
  }

  def impl: Seq[T] => Seq[T] = {
    val ret = if (dfg.isRecursive) implRecursive else implForwarding
    ret
  }

  def implAsComponent(inputWidths: Seq[BitCount] = Seq.fill(dfg.inputNodes.size)(10 bits)): Unit = {
    GenRTL(new Component {
      val dataIn: Flow[Vec[T]] = slave Flow Vec(inputWidths.map(holderProvider(_)))
      val dataOut: Flow[Vec[T]] = master Flow Vec(dfg.outputNodes.map(output => holderProvider(output.hardware.outWidths.head)))
      dataOut.payload := Vec(impl(dataIn.payload))
      dataOut.valid := Delay(dataIn.valid, dfg.latency, init = False)
    })
  }
}

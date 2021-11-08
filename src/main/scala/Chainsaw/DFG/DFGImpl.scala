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
class DFGImpl[T <: Data](dfg: DFGGraph[T], dataReset: Boolean = false)(implicit val holderProvider: BitCount => T) {

  val logger: Logger = LoggerFactory.getLogger(s"implementing procedure")
  logger.info(s"implementing dfg ${dfg.name}, dataReset = $dataReset")
  logger.info(s"number of nodes: ${dfg.innerNodes.size}, number of modules: ${dfg.innerNodes.map(_.hardware).distinct.size}")

  implicit def currentDFG: DFGGraph[T] = dfg

  // attributes tobe used
  val vertexSeq: Seq[DSPNode[T]] = dfg.vertexSeq
  val globalLcm: Int = dfg.globalLcm
  val inputNodes: Seq[DSPNode[T]] = dfg.inputNodes
  val outputNodes: Seq[DSPNode[T]] = dfg.outputNodes

  val hardware2component: Map[DSPHardware[T], () => Component with NodeComponent[T]] = dfg.innerNodes.map(_.hardware).distinct.map(hardware => hardware -> hardware.asComponent).toMap
  val componentMap: Map[DSPNode[T], () => Component with NodeComponent[T]] = dfg.innerNodes.map(node => node -> hardware2component(node.hardware)).toMap

  def initGlobalCount(): GlobalCount = {
    val globalCounter = CounterFreeRun(globalLcm)
    globalCounter.value.setName("globalCounter", weak = true)
    GlobalCount(globalCounter.value)
  }

  def implVertex(target: DSPNode[T], signalMap: mutable.Map[DSPNode[T], Seq[T]])(implicit globalCount: GlobalCount): Seq[T] = {

    val dataIns: Seq[Seq[DSPEdge[T]]] = target.incomingEdges.groupBy(_.inOrder).toSeq.sortBy(_._1).map(_._2)
    val dataInsOnPorts = dataIns.zipWithIndex.map { case (dataInsOnePort, i) => // combine dataIns at the same port by a mux
      //      val dataCandidates: Seq[T] = dataInsOnePort.map(edge => edge.hardware(signalMap(edge.source), edge.weight.toInt).apply(edge.outOrder))
      val dataCandidates: Seq[T] = dataInsOnePort.map { edge =>
        val dataIn = signalMap(edge.source)(edge.outOrder)
        if (dataReset) Delay(dataIn, edge.weight.toInt, init = dataIn.getZero)
        else Delay(dataIn, edge.weight.toInt)
      }
      val schedulesOnePort: Seq[Seq[Schedule]] = dataInsOnePort.map(_.schedules)
      val mux = DFGMUX[T](schedulesOnePort)
      val succeed = Try(mux.impl(dataCandidates, globalLcm))
      succeed match {
        case Failure(exception) => logger.error(s"MUX impl failed on:\n${dataInsOnePort.map(_.symbol).mkString("|")}")
          mux.impl(dataCandidates, globalLcm)
        case Success(value) => value
      }
    }
    //        target.hardware.impl(dataInsOnPorts, globalCount)
    // implement target using dataIns from different ports
    if (target.isInner && dfg.isForwarding) {
      logger.debug(s"implementing $target using submodule, inputWidths = ${dataInsOnPorts.map(_.getBitsWidth).mkString(" ")}")
      val nodeComponent = componentMap(target)()
      nodeComponent.dataIn := Vec(dataInsOnPorts)
      nodeComponent.dataOut
    }
    else target.hardware.impl(dataInsOnPorts, globalCount)

//    if (target.isInstanceOf[VirtualNode[T]]) {
//      val drivingEdge = target.incomingEdges.head
//      val sourceSignal = signalMap(target.sources.head).head
//      Seq(if (dataReset) Delay(sourceSignal, drivingEdge.delay, init = sourceSignal.getZero)
//      else Delay(sourceSignal, drivingEdge.delay))
//    }
//    else {
//
//    }
  }

  // implement a recursive graph
  def implRecursive: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    logger.info("implementing DFG by algo for recursive DFG")
    require(inputNodes.size == dataIns.size, "input size mismatch")
    val signalMap: mutable.Map[DSPNode[T], Seq[T]] = mutable.Map(vertexSeq.map { node => // a map to connect nodes with their outputs(placeholder)
      if (node.hardware.outWidths.exists(_.value == -1)) logger.warn(s"node $node has undetermined width ${node.hardware.outWidths.mkString(" ")} in a recursive DFG")
      node -> node.hardware.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i))
    }: _*)

    implicit val globalCount: GlobalCount = initGlobalCount()

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }
    vertexSeq.diff(inputNodes).foreach { target =>
      val rets = implVertex(target, signalMap)
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
    implicit val globalCount: GlobalCount = initGlobalCount()
    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap += node -> Seq(bits) }

    def implemented = signalMap.keys.toSeq

    def remained = vertexSeq.diff(implemented)

    def nextStageNodes = remained.filter(_.sources.forall(implemented.contains(_)))

    while (nextStageNodes.nonEmpty) {
      nextStageNodes.foreach { target =>
        val rets = implVertex(target, signalMap)
        signalMap += target -> rets
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

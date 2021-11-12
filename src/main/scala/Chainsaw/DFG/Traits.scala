package Chainsaw.DFG

import Chainsaw._
import org.slf4j.Logger
import spinal.core._

trait Transform[T <: Data] {

  val dfg: DFGGraph[T]

  def transformed: DFGGraph[T]

  val logger: Logger

//  def logIO() = {logger.info(s"\nbefore: \n\t${dfg.ioPositions.mkString("\n\t")}\n\tlatency -> ${dfg.latency}\n" +
//      s"after: \n\t${transformed.ioPositions.mkString("\n\t")}\n\tlatency -> ${transformed.latency}\n" +
//      s"latency: ${dfg.latency} -> ${transformed.latency}")
//  }

  def latencyTransformations: Seq[LatencyTrans]
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

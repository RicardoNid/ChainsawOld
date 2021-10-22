package Chainsaw.DFG

import Chainsaw._
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class DFGImplSoft[T](dfg: DFGGraph[T]) {
  val logger: Logger = LoggerFactory.getLogger(s"implementing procedure")
  // TODO: implement this for algorithm DFG
  def impl: Seq[T] => Seq[T] = (dataIns: Seq[T]) => dataIns
}

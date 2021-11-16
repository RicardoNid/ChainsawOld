package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._

//class NewUnfolding[T <: Data](override val dfg: DFGGraph[T], unfoldingFactor: Int) extends Transform[T] {
//
////  val newDevices =
//
//  override val transformName: String = "unfolding"
//
//  override def timeSpaceTransform(iteration: Iteration[T]): Iteration[T] = {
//
//  }
//
//  override def periodTransform(period: Int): Int = ???
//
//  override def iterationsInvolved: Seq[Iteration[T]] = ???
//}

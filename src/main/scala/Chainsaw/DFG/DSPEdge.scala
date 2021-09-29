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
import scala.collection.mutable.ArrayBuffer

abstract class DSPEdge {
  def impl(dataIn: Bits, delay: Int): Bits

  val schedules = ArrayBuffer(Schedule(1, 1))

  val order: Int
}

class FPGADelay(orderv: Int) extends DSPEdge {
  override def impl(dataIn: Bits, delay: Int): Bits = {
    if (dataIn.getBitsWidth >= 128 && delay >= 2) Delay(dataIn, delay) // TODO: replace this with a "FIFO function"
    else Delay(dataIn, delay)
  }

  override val order: Int = orderv

  override def toString: String = super.toString.takeRight(2)
}


object FPGADelay {
  def apply(orderv: Int): FPGADelay = new FPGADelay(orderv)
}

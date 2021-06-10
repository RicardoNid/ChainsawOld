package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class FIFO[T <: Data](dataType: HardType[T], depth: Int) extends StreamFifo(dataType, depth) {

  def init(): Unit = {
    io.push.valid := False
    io.pop.ready := False
    io.push.payload.assignFromBits(B(BigInt(0), io.push.payload.getBitsWidth bits))
  }

  def push(data: T): Unit = {
    io.push.valid := True
    io.push.payload := data.resized
  }

  def pop(): T = {
    io.pop.ready := True
    io.pop.payload
  }

}

object FIFO {
  def apply[T <: Data](dataType: HardType[T], depth: Int): FIFO[T] = new FIFO(dataType, depth)
}



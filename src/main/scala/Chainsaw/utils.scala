package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

trait EasyFIFO[T <: Data] {
  def init(): Unit

  def push(data: T): Unit

  def pop(): T
}

class FIFO[T <: Data](dataType: HardType[T], depth: Int) extends StreamFifo(dataType, depth) with EasyFIFO[T] {
  def init(): Unit = {
    io.push.valid := False
    io.pop.ready := False
    io.push.payload.assignFromBits(B(BigInt(0), io.push.payload.getBitsWidth bits)) // avoid latch
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

class FIFOLowLatency[T <: Data](dataType: HardType[T], depth: Int) extends StreamFifoLowLatency(dataType, depth, latency = 1) with EasyFIFO[T] {
  def init(): Unit = {
    io.push.valid := False
    io.pop.ready := False
    io.push.payload.assignFromBits(B(BigInt(0), io.push.payload.getBitsWidth bits)) // avoid latch
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

object FIFOLowLatency {
  def apply[T <: Data](dataType: HardType[T], depth: Int): FIFOLowLatency[T] = {
    val ret = new FIFOLowLatency(dataType, depth)
    ret.init()
    ret
  }
}

object FIFO {
  def apply[T <: Data](dataType: HardType[T], depth: Int): FIFO[T] = {
    val ret = new FIFO(dataType, depth)
    ret.init()
    ret
  }
}



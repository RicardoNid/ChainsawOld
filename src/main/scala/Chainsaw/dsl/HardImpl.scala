package Chainsaw.dsl

import spinal.core._

case class RawImpl(size: (Int, Int), impl: (Vec[Bits], Bool) => Vec[Bits], latency: Int)

abstract class HardImpl {

  //  def getLut(data: Array[BigInt], fold: Int) = {
  //
  //    (addr: UInt, last: Bool) => {
  //      val ROM = Mem(data.map(B(_)))
  //      val counter = CounterFreeRun(fold)
  //      when(last)(counter.clear())
  //      val combinedAddr = counter.value @@ addr
  //      val ret = ROM.readAsync(counter.value @@ addr)
  //      ret
  //    }
  //
  //  }

  val spaceFold: Array[Int]

  val timeFold: Array[Int]

  def getImpl(spaceFold: Int, timeFold: Int): RawImpl

}

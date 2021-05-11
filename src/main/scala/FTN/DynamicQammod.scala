package FTN

import Chainsaw._
import spinal.core.{Component, _}
import spinal.lib._

import scala.collection.mutable

class DynamicQammod(bitAlloc: IndexedSeq[Int]) extends Component {

  val QAMSymbols = mutable.Map[Int, IndexedSeq[(Double, Double)]]()
  QAMSymbols + (1 -> Array((1.0, 0.0), (-1.0, 0.0)))
  QAMSymbols + (4 -> Array((1.0, 0.0), (-1.0, 0.0)))

  def getQAMSymbol(bits: Bits) = {

    val bitsAllocated = bits.getBitsWidth
    val ret = (globalType, globalType)

    if (bitsAllocated == 4) {
      switch(bits) {
        (0 until 15).foreach { i =>
          is(U(i, 4 bits).asBits) {
            ret._1 := i.toDouble
            ret._2 := (-i).toDouble
          }
        }
        default {
          ret._1 := 0.0
          ret._2 := 0.0
        }
      }
    }
    ret
  }

  // check whether the bitAlloc is valid
  require(bitAlloc.length == DataCarrierPositions.length)
  require(bitAlloc.forall(i => i >= 0 && i <= 8))
  require(bitAlloc.sum == SubcarriersNum * BitsPerSymbolQAM)

  val bitsWidth = SubcarriersNum * BitsPerSymbolQAM
  //  val input = in Vec(Bool, bitsWidth) // bits to be mapped to QAM symbols
  val input = in Bits (bitsWidth bits) // bits to be mapped to QAM symbols
  val output = out Vec(globalType, SubcarriersNum * 2) // QAM symbols

  val bitLocations = (0 until SubcarriersNum).map(i => bitAlloc.take(i).sum)
  val groupedBits = bitLocations.zip(bitAlloc).map { case (start, length) => input.asBools.slice(start, start + length).asBits() }
  (0 until SubcarriersNum).foreach(i => groupedBits(i).setName(s"bitsBeforeMapping$i"))
  (0 until SubcarriersNum).foreach { i =>
    output(i * 2) := getQAMSymbol(groupedBits(i))._1
    output(i * 2).setName("QAM_real")
    output(i * 2 + 1) := getQAMSymbol(groupedBits(i))._2
    output(i * 2 + 1).setName("QAM_imag")
  }
}

object DynamicQammod {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new DynamicQammod(Array.ofDim[Int](SubcarriersNum).map(_ => 4)))
  }
}
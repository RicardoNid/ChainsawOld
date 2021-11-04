package Chainsaw.comm.qam

import Chainsaw.matlabIO._
import scala.collection.mutable.ArrayBuffer

object Algos {

  def qamdemod(input: MComplex, bitsAllocated: Int) = {

    def fold(value: Double, times: Int) = {
      val folded = ArrayBuffer[Double]()
      (0 until times).foreach { i =>
        if (i == 0) folded += value
        else folded += (folded.last.abs - (1 << (times - i)))
      }
      folded
    }

    def bits2Int(bits: Seq[Boolean]) = bits.reverse.zipWithIndex.map { case (bool, i) => if (bool) 1 << i else 0 }.sum

    val realAbs = input.real.abs
    val imagAbs = input.imag.abs

    val realLt = (value: Int) => (realAbs - value) < 0
    val imagLt = (value: Int) => (imagAbs - value) < 0
    val realLtImag = realAbs < imagAbs

    val bits = bitsAllocated match {
      case 5 =>
        val bit0 = input.real > 0
        val bit3 = input.imag < 0

        val bit1 = realLt(4) && imagLt(4)
        val bit2 = (!realLt(2) && imagLt(4)) || (!realLt(4) && !imagLt(4) && !realLtImag)
        val bit4 = imagLt(2) || (realLt(2) && !imagLt(4))
        Seq(bit0, bit1, bit2, bit3, bit4)
      case 7 =>
        val bit0 = input.real > 0 // diff = 64
        val bit4 = input.imag < 0 // diff = 4

        val bit1 = realLt(8) && imagLt(8) // diff = 32
        val bit2 = (!realLt(4) && imagLt(8)) || (!realLt(8) && !imagLt(8) && !realLtImag) // diff = 16 // TODO: diag
        val bit3 = (!realLt(2) && realLt(6)) || (!realLt(10) && !realLtImag) // diff = 8 // TODO: the "tiny triangle"
        val bit5 = imagLt(4) || (realLt(4) && !imagLt(8)) // diff = 2
        val bit6 = (!imagLt(2) && imagLt(6)) || (!imagLt(10) && realLtImag) // diff = 1

        Seq(bit0, bit1, bit2, bit3, bit4, bit5, bit6)

      case _ =>
        val realValues = fold(input.real, (bitsAllocated + 1) / 2)
        val imagValues = fold(input.imag, bitsAllocated / 2)
        ((realValues.head > 0) +: realValues.tail.map(_ < 0)) ++ imagValues.map(_ < 0)
    }

    bits2Int(bits)

  }
}


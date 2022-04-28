package Chainsaw.crypto.ReedSolomon
import spinal.core._
import spinal.lib._

import scala.collection.mutable._
object GaloisFieldHardWare {

  implicit class GFUIntOp(u: UInt) {
    val m  = u.getBitsWidth
    val gf = GaloisField(m)

    def add(that: UInt) = u ^ that

    def constMulti(that: Int) = {
      require(that < (1 << m) && that >= 0, "Out of Range!")
      if (that == 0)
        U(0, m bits)
      else if (that == 1)
        u
      else {
        val bufferArr = Array.fill(2 * m - 1)(ArrayBuffer[Int]())
        bufferArr.zipWithIndex.foreach { case (b, i) =>
          if (i < m)
            Range(0, i + 1).foreach(k => if (BigInt(that).testBit(i - k)) b.append(k))
          else
            Range(i - m + 1, m).foreach(k => if (BigInt(that).testBit(i - k)) b.append(k))
        }
        Range(m, 2 * m - 1).foreach { i =>
          val converted = BigInt(gf.alphaPowTable(i))
          Range(0, m).foreach { b =>
            if (converted.testBit(b)) {
              bufferArr(i).foreach(bi => if (bufferArr(b).contains(bi)) bufferArr(b).remove(bufferArr(b).indexOf(bi)) else bufferArr(b).append(bi))
            }
          }
        }
        Vec(bufferArr.take(m).map(b => b.map(u(_)).reduce(_ ^ _))).asBits.asUInt
      }
    }

    def multi(that: UInt): UInt = {
      val ret = UInt(m bits)
      ret := 0
      when(u === 1)(ret := that) elsewhen (that === 1)(ret := u)
      Range(2, 1 << m).foreach { i =>
        Range(i, 1 << m).foreach { j =>
          when((u === i && that === j) || (u === j && that === i))(ret := gf.multi(i, j))
        }
      }
      ret
    }

    def inv: UInt = {
      val ret = UInt(m bits)
      switch(u) {
        is(0)(ret := U(0, m bits))
        Range(1, 1 << m).foreach(i => is(i)(ret := U(gf.getInverse(i), m bits)))
      }
      ret
    }
  }
}

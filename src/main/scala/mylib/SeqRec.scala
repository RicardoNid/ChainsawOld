// a general sequence recognizer
package mylib

import mylib.SeqRec.countBack
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random

class SeqRec(sequence: String, data: Bool) extends ImplicitArea[Bool] {

  val length = sequence.length
  val seq = sequence.map(char => if (char == '1') True else False)

  val willClear = False.allowOverride
  val willRecognize = False.allowOverride
  val recognized = False.allowOverride

  val count = Reg(UInt(log2Up(length + 1) bits)) init (U(0))

  val countNext = (i: Int) => {
    when(data === seq(i))(count := count + U(1))
      .otherwise(count := U(countBack(sequence, i)))
  }

  when(willClear)(count := U(0))
    .otherwise(for (i <- 0 until length) when(count === U(i))(countNext(i)))

  willRecognize := (count === U(length - 1) && (data === seq(length - 1)))
  recognized := (count === U(length))

  def clear(): Unit = willClear := True

  override def implicitValue: Bool = this.recognized
}

object SeqRec {

  def apply(sequence: String, data: Bool): SeqRec = new SeqRec(sequence, data)

  def apply(sequence: String, data: Bool, clear: Bool): SeqRec = {
    val seqRec = SeqRec(sequence, data)
    when(clear) {
      seqRec.clear()
    }
    seqRec
  }

  // find out the state to go back when next state is not avaiable, current state = i
  def countBack(sequence: String, i: Int): Int = {
    val inv = (char: Char) => if (char == '1') '0' else '1'
    require(i >= 0 && i < sequence.length)
    val wrongBit = inv(sequence(i))
    val subSeqB = sequence.slice(0, i) + inv(sequence(i))
    println(subSeqB)
    val subSeqA = sequence.slice(0, i)
    // check in reverse order
    for (last <- Range(1, i + 1).reverse) {
      val candidate = sequence.slice(0, last)
      if (candidate.last == wrongBit) {
        if (candidate == subSeqB.slice(i + 1 - candidate.length, i + 1)) return candidate.length
      }
    }
    return 0
  }
}
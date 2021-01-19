package sysu.util

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import sysu.util.SeqGen.encode
import sysu.xilinx._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait SeqGenMode

case object FSM extends SeqGenMode

case object COUNT extends SeqGenMode

case object LUT extends SeqGenMode

case object AUTO extends SeqGenMode

class SeqGen(sequence: Seq[Int], mode: SeqGenMode = FSM) extends ImplicitArea[UInt] {

  val length = sequence.length
  val bitWidth = log2Up(sequence.max + 1)

  val output = out UInt (bitWidth bits)

  mode match {
    case FSM => { // 以真值表方式实现
      // encoding
      require(sequence.forall(_ >= 0), "SeqGen requires a sequence of unsigned(>=0) integers")
      val encodedSequence = encode(sequence)
      val encodedBitWidth = log2Up(encodedSequence.max)
      val encodedSequenceVec = Vec(UInt(encodedBitWidth bits), length)
      (0 until length).foreach(i => encodedSequenceVec(i) := U(encodedSequence(i)).resized)

      val sequenceReg = Reg(UInt(encodedBitWidth bits)) init (encodedSequenceVec(0))
      switch(sequenceReg) {
        for (i <- 0 until length - 1) is(encodedSequenceVec(i))(sequenceReg := encodedSequenceVec(i + 1))
        is(encodedSequenceVec(length - 1))(sequenceReg := encodedSequenceVec(0))
        default(sequenceReg := sequenceReg)
      }
      output := sequenceReg.resize(bitWidth)
    }

    case LUT => { // 以查表方式实现
      val sequenceVec = Vec(UInt(bitWidth bits), length)
      (0 until length).foreach(i => sequenceVec(i) := U(sequence(i)).resized)

      val rom = Mem(UInt(bitWidth bits), sequenceVec)

      val addrCount = Counter(length, True)
      output := rom(addrCount)
    }
  }

  override def implicitValue: UInt = output
}

object SeqGen {

  def apply(sequence: Seq[Int], mode: SeqGenMode) = new SeqGen(sequence, mode)

  // optimization : 对SeqGen的优化通过优化编码函数实现
  // 当前编码方案
  // 设最大重复次数为n,增加log2Up(n)位编码消除重复
  def encode(sequence: Seq[Int]): Seq[Int] = {

    if (sequence.distinct.length == sequence.length) sequence
    else {
      val repeatCount = sequence.zipWithIndex.map(tuple => {
        val ele = tuple._1
        val i = tuple._2
        sequence.slice(0, i).count(_ == ele)
      })
      //      repeatCount.foreach(println(_))
      val extraBitWidth = log2Up(repeatCount.max)
      val originalMax = 1 << log2Up(sequence.max)
      (0 until sequence.length).map(i => repeatCount(i) * originalMax + sequence(i))
    }
  }

  def main(args: Array[String]): Unit = { // demo
    val report = VivadoFlow(
      design = new SeqGenModule(Array(1, 2, 3, 4), LUT),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(topModuleName = "SeqGen", workspacePath = "output/seqgen", frequencyTarget = 400 MHz, taskType = SYNTH),
      force = true).doit()
    report.printArea
    report.printFMax
    println(report.getReport)
  }
}

object testSeqGen {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new SeqGenModule(Array(1, 2, 3, 4), LUT)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        sleep(100)
        simSuccess()
      }
  }
}
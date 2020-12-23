package sysu.util

import spinal.core._
import spinal.core.sim._
import sysu.util.SeqGen.encode
import sysu.xilinx._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// todo : 实现一个工厂方法,接收迭代器
// todo : 编码应该在外部实现完毕,不用放到内部做
class SeqGen(sequence: Seq[Int]) extends Component {

  // encoding
  require(sequence.forall(_ >= 0), "SeqGen requires a sequence of unsigned(>=0) integers")
  val length = sequence.length
  val encodedSequence = encode(sequence)
  val bitWidth = log2Up(sequence.max)
  val encodedBitWidth = log2Up(encodedSequence.max)
  val encodedSequenceVec = Vec(UInt(encodedBitWidth bits), length)
  //  sequence.foreach(println(_))
  //  encodedSequence.foreach(println(_))
  (0 until length).foreach(i => encodedSequenceVec(i) := U(encodedSequence(i)).resized)

  // todo : 重构,尽量避免resize
  val io = new Bundle {
    val output = out UInt (bitWidth bits)
  }

  val sequenceReg = Reg(UInt(encodedBitWidth bits)) init (encodedSequenceVec(0))

  switch(sequenceReg) {
    for (i <- 0 until length - 1) is(encodedSequenceVec(i))(sequenceReg := encodedSequenceVec(i + 1))
    is(encodedSequenceVec(length - 1))(sequenceReg := encodedSequenceVec(0))
    default(sequenceReg := sequenceReg)
  }

  io.output := sequenceReg.resize(bitWidth)
}

object SeqGen {

  def apply(sequence: Seq[Int]) = new SeqGen(sequence)

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

  def verilogPostProcess(content: String) = content.replace("wire       [8:0]    dontCare;", "")
    .replace("assign dontCare = 9'h1f4;", "")
    .replace("dontCare", "\'x")
}

object testSeqGen {

  val period = 2

  def main(args: Array[String]): Unit = {

    import sysu.CNN._

    val seq = addrSeq(5, 7, 7, 3, 3, 3, true)
    val sequence = getColArrays(seq)(0)

    val randGen = new Random(42)
    //    val sequence = Array.ofDim[Int](100).map(_ => randGen.nextInt % 100 + 500)
    SimConfig.withWave.compile(new SeqGen(sequence)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}

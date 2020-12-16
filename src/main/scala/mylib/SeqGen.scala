package mylib

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import scala.util.Random
import glob._

class SeqGen(sequence: Seq[Int]) extends Component { // todo : 找一种比Array更好的数据类型

  // encoding
  // todo : 补充序列重复检查
  require(sequence.forall(_ >= 0))
  val length = sequence.length

  val bitWidth = log2Up(sequence.max)
  val counterWidth = log2Up(length)
  val encodedSequence = Vec(UInt(bitWidth + counterWidth bits), length)

  var actualWidth = 0
  val noReplicate = sequence.length == sequence.distinct.length

  if (noReplicate) { // 没有重复项,直接编码序列值
    (0 until length).foreach(i => encodedSequence(i) := U(sequence(i), bitWidth bits))
    actualWidth = bitWidth
  }
  else { // 有重复项,还要编码计数器值到低位 todo : 实现按重复次数编码(总之,更优编码) todo : 搞清楚截断高位还是低位
    (0 until length).foreach(i => encodedSequence(i) := U(i, counterWidth bits) @@ U(sequence(i), bitWidth bits))
    actualWidth = bitWidth + counterWidth
  }

  // todo : 重构,尽量避免resize
  val io = new Bundle {
    val output = out UInt (bitWidth bits)
  }

  val sequenceReg = Reg(UInt(actualWidth bits)) init (encodedSequence(0).resized)

  switch(sequenceReg) {
    for (i <- 0 until length - 1) is(encodedSequence(i).resize(actualWidth))(sequenceReg := encodedSequence(i + 1).resize(actualWidth))
    is(encodedSequence(length - 1).resize(actualWidth))(sequenceReg := encodedSequence(0).resize(actualWidth))
    default(sequenceReg := U(0))
  }

  io.output := sequenceReg.resize(bitWidth)
}

object SeqGen {


  def main(args: Array[String]): Unit = {

    //    val randGen = new Random(42)
    //    val sequence = Array.ofDim[Int](100).map(_ => randGen.nextInt % 100 + 500)
    //
    //    SpinalConfig(mode = SystemVerilog, targetDirectory = "output/mylib").generate(new SeqGen(sequence))

    val report = VivadoFlow(
      // todo : vivado flow infos
      workspacePath = "output/VIVADO",
      toplevelPath = "output/mylib/SeqGen.sv"
    )
    println(report.getArea)
    println(report.getFMax() / 1E6 + "MHz")
  }
}

object testSeqGen {

  val period = 2

  def main(args: Array[String]): Unit = {

    val randGen = new Random(42)
    val sequence = Array.ofDim[Int](100).map(_ => randGen.nextInt % 100 + 500)
    sequence.foreach(println(_))
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

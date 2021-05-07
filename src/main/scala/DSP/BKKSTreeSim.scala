package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._

class BKKSTreeSim extends Component with DSPSim[Vec[UInt], Vec[UInt], Array[Int], Array[Int]] {
  override val input: Flow[Vec[UInt]] = slave Flow Vec(UInt(10 bits), 16)
  override val output: Flow[Vec[UInt]] = master Flow Vec(UInt(10 bits), 16)

  val add = (x: UInt, y: UInt) => x + y

  val bkTree = new BKKSTree[UInt](input.payload, add, 0)
  output.payload := bkTree.implicitValue
  output.valid := RegNext(input.valid)
  output.valid.init(False)

  override val timing: TimingInfo = bkTree.getTimingInfo

  override def poke(testCase: Array[Int], input: Vec[UInt]): Unit = {
    testCase.zip(input).foreach { case (data, port) => port #= data }
    clockDomain.waitSampling()
  }

  override def peek(output: Vec[UInt]): Array[Int] = {
    val ret = output.map(_.toInt).toArray
    clockDomain.waitSampling()
    ret
  }

  override def referenceModel(testCase: Array[Int]): Array[Int] =
    (0 until testCase.length).map(i => testCase.takeRight(i + 1).sum).reverse.toArray

  override def isValid(refResult: Array[Int], dutResult: Array[Int]): Boolean =
    refResult.zip(dutResult).forall(pair => pair._1 == pair._2)

  override def messageWhenInvalid(testCase: Array[Int], refResult: Array[Int], dutResult: Array[Int]): String =
    s"${refResult.zip(dutResult).mkString("\n")}"

  override def messageWhenValid(testCase: Array[Int], refResult: Array[Int], dutResult: Array[Int]): String =
    s"${refResult.zip(dutResult).mkString("\n")}"

  override type RefOwnerType = this.type
}

object BKKSTreeSim {
  def main(args: Array[String]): Unit = {

    debug = true

    SimConfig.compile(new BKKSTreeSim).doSim { dut =>
      dut.sim()
      dut.insertTestCase((0 until 16).toArray)
      val report = dut.simDone()
      println(report.validLog)
    }
  }
}



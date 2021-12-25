package Chainsaw.comm.viterbi

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class GetMinWithIndex() extends Component with DSPTestable[Vec[UInt], Vec[UInt]] {

  private def getMinWithIndex(data: Seq[UInt], width:Int): UInt = {
    val N = data.length
    if (data.length == 1) data.head
    else {
      val (data0, data1) = data.splitAt(N / 2)
      val ret = data0.zip(data1).map { case (left, right) =>
        val bit = right.takeHigh(width).asUInt <= left.takeHigh(width).asUInt
        Mux(bit, right, left) @@ bit
      }
      getMinWithIndex(ret, width)
    }
  }

  override val dataIn = slave Flow Vec(UInt(16 bits), 16)
  override val dataOut = master Flow Vec(UInt(16 bits), 2)

  val ret = getMinWithIndex(dataIn.payload, 16)
  dataOut.payload(0) := RegNext(ret.takeHigh(16)).asUInt
  dataOut.payload(1) := RegNext(ret.takeLow(ret.getBitsWidth - 16)).asUInt.resized

  override val latency = 1
  dataOut.valid := RegNext(dataIn.valid, init = False)
}

object GetMinWithIndex extends App {
  val testCase = (0 until 16).map(_ => ChainsawRand.nextBigInt(16))
  val min = testCase.min
  val index = BigInt(testCase.indexWhere(_ == min))
  doFlowPeekPokeTest("testGetMin", GetMinWithIndex(), Seq(testCase), Seq(Seq(min, index)))
}

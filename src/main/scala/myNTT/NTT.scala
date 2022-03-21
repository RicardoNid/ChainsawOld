package myNTT

import Chainsaw.DFG._
import Chainsaw._
import spinal.core._

//并行度决定了输入输出端口的数量，输入输出端口数为2^(parallelDegree+1)
//折叠系数决定了每几个周期在所有输入端口输入一次数据

class NTT(
    N: BigInt, //被模常数
    w: BigInt, //原根w1
    numOfNTT: Int, //变换点数
    parallelDegree: Int = 0, //并行度，范围[0,log2Up(numOfNTT)-1]
    foldNum: Int        = 1 //折叠系数，应该是log2Up(numOfNTT)的因数
) extends Component {
  require(
    isPow2(numOfNTT) && (N & 1) == 1 && parallelDegree >= 0 &&
      parallelDegree <= log2Up(numOfNTT) - 1 && foldNum > 0 &&
      log2Up(numOfNTT) % foldNum == 0
  )

  val n     = log2Up(N)
  val IONum = 2 << parallelDegree

  val dataIn  = Array.fill(IONum)(in UInt (n bits))
  val dataOut = Array.fill(IONum)(out UInt (n bits))

  val constArray = Array.fill(numOfNTT >> 1)(UInt(n bits))
  var w0         = (BigInt(1) << n) - N
  constArray.foreach { c =>
    c := U(w0)
    w0 = (w0 * w) % N
  }
  val fftG = new DIF_FFTGraph[UInt](
    "FFTGraph",
    UInt(n bits),
    ButterflyHardware(NTTButterfly(N, _, _, _), n bits, n + 3 cycles),
    ButterflyHardware(switch[UInt], n bits),
    numOfNTT,
    parallelDegree,
    constArray
  )

  var delay = 0

  if (foldNum != 1) {
    val butterflyNodes = fftG.getButterflyNodes
    val SwitchNodes    = fftG.getSwitchNodes

    val foldSet = new Array[Seq[DSPNode[UInt]]]((IONum >> 1) * 3 * log2Up(numOfNTT) / foldNum)
    for (i <- 0 until (log2Up(numOfNTT), foldNum)) {
      for (j <- 0 until (IONum >> 1)) {
        val sw1Set       = new Array[DSPNode[UInt]](foldNum)
        val butterflySet = new Array[DSPNode[UInt]](foldNum)
        val sw2Set       = new Array[DSPNode[UInt]](foldNum)
        for (k <- 0 until foldNum) {
          sw1Set(k)       = SwitchNodes(2 * (i + k))(j)
          butterflySet(k) = butterflyNodes(i + k)(j)
          sw2Set(k)       = SwitchNodes(2 * (i + k) + 1)(j)
        }
        foldSet(i / foldNum * 3 * (IONum >> 1) + j * 3)     = sw1Set
        foldSet(i / foldNum * 3 * (IONum >> 1) + j * 3 + 1) = butterflySet
        foldSet(i / foldNum * 3 * (IONum >> 1) + j * 3 + 2) = sw2Set
      }
    }
    val fftG2 = fftG.folded(foldSet)
    val out   = fftG2.impl(dataIn)
    dataOut.indices.foreach(i => dataOut(i) := out(i))
    delay = fftG2.latency
  } else {
    val out = fftG.impl(dataIn)
    dataOut.indices.foreach(i => dataOut(i) := out(i))
    delay = fftG.latency
  }

  def getDelay: Int = delay
}

object NTT {
  def main(arg: Array[String]): Unit = {
    val N: BigInt = 3329
    val numOfNTT  = 256
    var w: BigInt = 1
    for (_ <- 0 until 13 * 256 / numOfNTT) {
      w = (w * 3) % N
    }
    GenRTL(new NTT(N, w, numOfNTT, 0, 1))
  }
}

package myNTT

import Chainsaw.DFG._
import numberFunction._
import spinal.core._

//FFT结构图

class DIF_FFTGraph[T <: Data](
    name: String,
    dataType: HardType[T],
    butterflyHardware: ButterflyHardware[T], //蝶形器件
    switchHardware: ButterflyHardware[T], //选择器件
    N: Int, //变换点数
    parallelDegree: Int = 0, //并行度，范围[0,log2Up(N)-1]
    constArray: Array[T] //常数数组
) extends DFGGraph[T](name) {
  require(
    isPow2(N) && N > 1 && constArray.length == (N >> 1) &&
      parallelDegree >= 0 && parallelDegree <= log2Up(N) - 1
  )

  val IONum = 2 << parallelDegree

  val dataIn  = Array.fill(IONum)(addInput()) //输入节点
  val dataOut = (0 until IONum).map(i => new OutputNode[T](s"output$i"))

  dataOut.foreach(d => addVertex(d))

  val n = log2Up(N)
  val p = new FFTNetSim(N, IONum).doDIFSim

  val switchNodes = (0 until (n << 1)).map { i =>
    (0 until (IONum >> 1)).map { j =>
      val w = switchHardware.asDeviceNode(s"switch${i}_$j")
      addVertex(w)
      w
    }
  }

  val butterflyNodes = (0 until n).map { i =>
    (0 until (IONum >> 1)).map { j =>
      val b = butterflyHardware.asDeviceNode(s"butterfly${i}_$j")
      addVertex(b)
      b
    }
  }

  val constNodes = (0 until (N >> 1)).map { i =>
    val c = ConstantNode(s"constant$i", constArray(i))
    addVertex(c)
    c
  }

  val zero = dataType()
  zero.assignFromBits(B(0, zero.getBitsWidth bits))
  val zeroNode = ConstantNode("constantZero", zero)
  addVertex(zeroNode)

  val one = dataType()
  one.assignFromBits(B(1, one.getBitsWidth bits))
  val oneNode = ConstantNode("consantOne", one)
  addVertex(oneNode)

  var delay       = 0
  val dataPresent = new Array[DSPNodeWithOrder[T]](IONum)
  var linkList    = new Array[Int](IONum)
  linkList.indices.foreach(i => linkList(i) = reverse(i, parallelDegree + 1))
  var beforeNum     = 0
  val withBeforeNum = Array.fill(IONum)(false)

  //输入端
  dataPresent.indices.foreach(i => dataPresent(i) = dataIn(i)(0))
  //内部连线
  for (i <- 0 until n) {
    val switchOn1  = (0 until p(i)._1).map(j => Schedule((j + delay) % (p(i)._1 << 1), p(i)._1 << 1))
    val switchOff1 = (0 until p(i)._1).map(j => Schedule((j + p(i)._1 + delay) % (p(i)._1 << 1), p(i)._1 << 1))
    val constantSw = p(i)._2(0).indices.map(j => Schedule((delay + p(i)._1 + j) % p(i)._2(0).length, p(i)._2(0).length))
    val switchOn2 = (0 until p(i)._3).map(j =>
      Schedule(
        (j + delay + p(i)._1 + butterflyHardware.delay) %
          (p(i)._3 << 1),
        p(i)._3    << 1
      )
    )
    val switchOff2 = (0 until p(i)._3).map(j =>
      Schedule(
        (j + p(i)._3 + delay + p(i)._1 + butterflyHardware.delay) %
          (p(i)._3 << 1),
        p(i)._3    << 1
      )
    )
    for (j <- 0 until (IONum >> 1)) {
      addEdge(dataPresent(linkList(2 * j)), switchNodes(2 * i)(j)(0), if (withBeforeNum(linkList(2 * j))) beforeNum else 0)
      addEdge(dataPresent(linkList(2 * j + 1)), switchNodes(2 * i)(j)(1), if (withBeforeNum(linkList(2 * j + 1))) beforeNum + p(i)._1 else p(i)._1)
      addEdge(switchNodes(2 * i)(j)(0), butterflyNodes(i)(j)(0), p(i)._1)
      addEdge(switchNodes(2 * i)(j)(1), butterflyNodes(i)(j)(1), 0)
      if (p(i)._1 > 0) {
        addEdge(oneNode(0), switchNodes(2 * i)(j)(2), 0, switchOn1)
        addEdge(zeroNode(0), switchNodes(2 * i)(j)(2), 0, switchOff1)
      } else {
        addEdge(oneNode(0), switchNodes(2 * i)(j)(2), 0)
      }
      for (k <- p(i)._2(j).indices) {
        addEdge(constNodes(p(i)._2(j)(k))(0), butterflyNodes(i)(j)(2), 0, Seq(constantSw(k)))
      }
      addEdge(butterflyNodes(i)(j)(0), switchNodes(2 * i + 1)(j)(0), 0)
      addEdge(butterflyNodes(i)(j)(1), switchNodes(2 * i + 1)(j)(1), p(i)._3)
      if (p(i)._3 > 0) {
        addEdge(oneNode(0), switchNodes(2 * i + 1)(j)(2), 0, switchOn2)
        addEdge(zeroNode(0), switchNodes(2 * i + 1)(j)(2), 0, switchOff2)
      } else {
        addEdge(oneNode(0), switchNodes(2 * i + 1)(j)(2), 0)
      }
      dataPresent(linkList(2 * j))       = switchNodes(2 * i + 1)(j)(0)
      withBeforeNum(linkList(2 * j))     = true
      dataPresent(linkList(2 * j + 1))   = switchNodes(2 * i + 1)(j)(1)
      withBeforeNum(linkList(2 * j + 1)) = false
    }
    delay += p(i)._1 + butterflyHardware.delay + p(i)._3
    linkList  = p(i)._4
    beforeNum = p(i)._3
  }

  for (i <- 0 until IONum) {
    addLatencyEdge(dataIn(0), dataOut(i))
    addEdge(dataPresent(linkList(i)), dataOut(i)(0), if (withBeforeNum(linkList(i))) beforeNum else 0)
  }

  setLatency(delay)

  def getButterflyNodes: Seq[Seq[ButterflyNode[T]]] = butterflyNodes

  def getSwitchNodes: Seq[Seq[DSPNode[T]]] = switchNodes

}

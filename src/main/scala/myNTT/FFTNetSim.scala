package myNTT

import myNTT.numberFunction._
import spinal.core._

import scala.language.postfixOps

//FFT网络的仿真
//通过模仿数据的顺序变化行为来得到网络的最终结构
//得出的结构会作为参数传回

class FFTNetSim(N: Int, numOfInput: Int) {
  require(isPow2(N) && isPow2(numOfInput) && numOfInput > 1 && N >= numOfInput)

  val layer = log2Up(N)
  val noOperationLayer = log2Up(numOfInput)
  val operationLayer = layer - noOperationLayer
  val normalOperationLayer = (operationLayer + 1) >> 1

  val numOfTotal = N / 2

  val period = 1 << operationLayer

  val simNet = new Array[Array[Int]](numOfInput) //每个元素是一个输入通道
  simNet.indices.foreach { i =>
    simNet(i) = (i until(N, numOfInput)).toArray
  }

  def doSwitch(row: (Int, Int), p: Int): Unit = {
    for (i <- 0 until(period, 2 * p)) {
      for (j <- 0 until p) {
        val temp = simNet(row._2)(i + j)
        simNet(row._2)(i + j) = simNet(row._1)(i + p + j)
        simNet(row._1)(i + p + j) = temp
      }
    }
  }

  def doDIFSim: Array[(Int, Array[Array[Int]], Int, Array[Int])] = {
    //执行DIF仿真并获得仿真过程列表
    //第一，三个参数为Switch
    //第二个参数为参数列表
    //第四个参数为连接列表
    //输入的连接默认是做过变换的
    val output = new Array[(Int, Array[Array[Int]], Int, Array[Int])](layer)
    val linkList = new Array[Int](numOfInput)
    for (i <- linkList.indices) {
      linkList(i) = reverse(i, noOperationLayer)
    }
    val paraList = new Array[Array[Int]](numOfInput / 2)
    for (i <- 0 until layer) {
      val jumpNum = 1 << i
      if (i >= operationLayer) {
        for (j <- 0 until numOfInput / 2) {
          paraList(j) = new Array[Int](if ((period >> i) <= 1) 1 else period)
          if ((period >> i) <= 1) {
            paraList(j)(0) = (simNet(linkList(2 * j))(0) * jumpNum) % numOfTotal
          } else {
            for (k <- 0 until period) {
              paraList(j)(k) = (simNet(linkList(2 * j))(k) * jumpNum) % numOfTotal
            }
          }
        }
        if (i == layer - 1) {
          for (j <- linkList.indices) {
            linkList(j) = reverse(j, noOperationLayer)
          }
        } else {
          val jLimit = 1 << (i - operationLayer + 1)
          val kLimit = numOfInput >> (i - operationLayer + 1)
          val bitNum = noOperationLayer - (i - operationLayer + 1)
          for (j <- 0 until jLimit) {
            for (k <- 0 until kLimit) {
              linkList(j * kLimit + k) = reverse(k, bitNum) + j * kLimit
            }
          }
        }
        output(i) = (0, paraList.clone(), 0, linkList.clone())
      } else if (i >= normalOperationLayer) {
        for (j <- 0 until numOfInput / 2) {
          paraList(j) = new Array[Int](if ((period >> i) <= 1) 1 else period)
          if ((period >> i) <= 1) {
            paraList(j)(0) = (simNet(linkList(2 * j))(0) * jumpNum) % numOfTotal
          } else {
            for (k <- 0 until period) {
              paraList(j)(k) = (simNet(linkList(2 * j))(k) * jumpNum) % numOfTotal
            }
          }
          doSwitch((linkList(2 * j), linkList(2 * j + 1)), 1 << i)
        }
        output(i) = (0, paraList.clone(), 1 << i, linkList.clone())
      } else {
        for (j <- 0 until numOfInput / 2) {
          doSwitch((linkList(2 * j), linkList(2 * j + 1)), 1 << (operationLayer - i - 1))
          paraList(j) = new Array[Int](if ((period >> i) <= 1) 1 else period)
          if ((period >> i) <= 1) {
            paraList(j)(0) = (simNet(linkList(2 * j))(0) * jumpNum) % numOfTotal
          } else {
            for (k <- 0 until period) {
              paraList(j)(k) = (simNet(linkList(2 * j))(k) * jumpNum) % numOfTotal
            }
          }
          doSwitch((linkList(2 * j), linkList(2 * j + 1)), 1 << i)
        }
        output(i) = (1 << (operationLayer - i - 1), paraList.clone(), 1 << i, linkList.clone())
      }
    }
    output
  }

  def print: Unit = {
    val width = simNet.map(s => s.max).max.toString.length + 3
    for (i <- simNet.indices) {
      for (j <- simNet.head.indices) {
        printf(s"%${width}d", simNet(i)(j))
      }
      printf("\n")
    }
  }

  def printReverse: Unit = {
    val width = simNet.map(s => s.max).max.toString.length + 3
    for (i <- simNet.indices) {
      for (j <- simNet.head.indices) {
        printf(s"%${width}d", reverse(simNet(reverse(i, noOperationLayer))(j), layer))
      }
      printf("\n")
    }
  }
}
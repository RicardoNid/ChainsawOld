package myNTT

import Chainsaw._
import cc.redberry._
import cc.redberry.rings.scaladsl._
import spinal.core.sim._

import scala.collection.mutable._
import scala.util.Random._

//仿真，目前只支持256点（因为golden函数）
//TODO:折叠后的仿真结果是错误的！

object NTTSim {
  def main(arg: Array[String]): Unit = {
    val N: BigInt = 3329
    val numOfNTT = 256
    var w: BigInt = 1
    for (_ <- 0 until 13 * 256 / numOfNTT) {
      w = (w * 3) % N
    }
    val ring = asRing(rings.Rings.Zp64(N.toLong))
    val ntt = crypto.NTT(ring, numOfNTT)
    val queue = new Queue[Long]
    val input = new Array[Long](numOfNTT)
    val output = new Array[Long](numOfNTT)
    val realOutput = new Array[Long](numOfNTT)
    val n = 10
    val parallelDegree = 6
    val foldNum = 4
    SimConfig.withWave.compile(new NTT(N, w, numOfNTT, parallelDegree, foldNum)).doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(10)

      fork {
        while (true) {
          input.indices.foreach(i => input(i) = nextInt(N.toInt))
          val output = ntt.NTT(input)
          output.foreach(queue.enqueue(_))
          for (i <- 0 until numOfNTT / dut.dataIn.length) {
            dut.dataIn.indices.foreach(j => dut.dataIn(j) #= input(i * dut.dataIn.length + j))
            dut.clockDomain.waitSampling(foldNum)
          }
        }
      }

      fork {
        dut.clockDomain.waitSampling(dut.getDelay)
        for (i <- 0 until n) {
          output.indices.foreach(i => output(i) = queue.dequeue())
          for (j <- 0 until numOfNTT / dut.dataOut.length) {
            dut.clockDomain.waitSampling(1)
            for (k <- dut.dataOut.indices) {
              realOutput(k * numOfNTT / dut.dataOut.length + j) = dut.dataOut(k).toLong
            }
            dut.clockDomain.waitSampling(foldNum - 1)
          }
          if (!output.sameElements(realOutput)) {
            printf(s"第${i}次出错，正确结果是：\n")
            print(output.toString)
            printf("你的结果是：\n")
            print(realOutput.toString)
          }
        }
        simSuccess()
      }
    }
  }
}
package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import breeze.linalg._
import breeze.numerics._
import breeze.signal._

case class CNNModel( // 高度简化的参数
                     L: Int = 13,
                     N: DenseVector[Int] = DenseVector(64, 64, 128, 128, 256, 256, 256, 512, 512, 512, 512, 512, 512),
                     C: DenseVector[Int] = DenseVector(3, 64, 64, 128, 128, 256, 256, 256, 512, 512, 512, 512, 512),
                     H: DenseVector[Int] = DenseVector(224, 224, 112, 112, 56, 56, 56, 28, 28, 28, 14, 14, 14),
                     W: DenseVector[Int] = DenseVector(224, 224, 112, 112, 56, 56, 56, 28, 28, 28, 14, 14, 14),
                     K: DenseVector[Int] = DenseVector(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
                     P: DenseVector[Int] = DenseVector(Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                     S: DenseVector[Int] = DenseVector(Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                     Pool: DenseVector[Boolean] = DenseVector(Array(false, true, false, true, false, false, true, false, false, true, false, false, true)),
                     PK: DenseVector[Int] = DenseVector(Array(2, 2, 2, 2, 2)),
                     PS: DenseVector[Int] = DenseVector(Array(1, 1, 1, 1, 1)),
                     dspCount: Int = 1728, // ZCU104
                     BRAMCount: Int = 624, // ZCU104
                     dspFactor: Int = 1
                   ) {
  val W_h = DenseVector(Array(5, 1, 1, 2, 1, 2, 2, 1, 27, 27, 6, 6, 6))
  val W_w = DenseVector(Array(4, 9, 29, 29, 29, 29, 29, 29, 15, 15, 17, 17, 17))
  val I_w = DenseVector(Array(1, 45, 7, 7, 7, 7, 7, 7, 1, 1, 1, 1, 1))


  require(C.length == N.length && H.length == N.length && K.length == N.length)
  val layerCount = N.length

  def div(a: Int, b: Int) = ceil(a.toDouble / b.toDouble)

  def ops(i: Int) = N(i) * C(i) * K(i) * K(i) * H(i) * H(i)

  def opsAll = (0 until layerCount).map(ops(_)).reduce(_ + _)

  def cycles(i: Int, abc: Tuple3[Int, Int, Int]) // formula : 3-8
  = div(N(i), abc._1) * div(C(i) * K(i) * K(i), abc._2) * div(H(i) * H(i), abc._3)

  def cycleBound = opsAll / (dspCount * dspFactor)

  def getABCOriginal = {} // 原始约束,以频率->行时间作为约束

  def getLayerParam(i: Int) = (N(i), C(i), H(i), W(i), K(i), P(i), S(i))

}

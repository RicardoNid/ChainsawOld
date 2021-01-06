package projects

import sysu.CNN._
import sysu.Opt._

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import sysu.xilinx._
import sysu.util._
import sysu.CNN._

package object Hwj {

  val dataWidth = 8
  val weightWidth = 8

  case class CNNModel(L: Int,
                      Hin: Array[Int], Hout: Array[Int], C: Array[Int], N: Array[Int],
                      K: Array[Int], S: Array[Int], Pad: Array[Int],
                      PK: Array[Int], PS: Array[Int]) {

    val Wh = Array[Int]()
    val Ww = Array[Int]()
    val Iw = Array[Int]()

    val layers = (0 until L).map(i =>
      Layer(Hin = Hin(i), Hout = Hout(i), C = C(i), N = N(i),
        K = K(i), S = S(i), Pad = Pad(i),
        PK = PK(i), PS = PS(i),
        Wh = Wh(i), Ww = Ww(i), Iw = Iw(i)))


    def formula3_13 = layers.map(layer => layer.K + layer.S).sum

    def formula3_14 = layers.map(layer => layer.formula3_12 * (layer.K + layer.S)).sum


  }

  case class Layer(Hin: Int, Hout: Int, C: Int, N: Int,
                   K: Int, S: Int, Pad: Int,
                   PK: Int, PS: Int,
                   Wh: Int, Ww: Int, Iw: Int) {
    val dataGrid = Array(Hin, Hin, C)

    def ceilDiv(a: Int, b: Int) = (roundUp(a, b) / b).toInt

    val Ih = Ww
    val blockedWh = ceilDiv(N, Wh)
    val blockedWw = ceilDiv(C * K * K, Ww)
    val blockedIw = ceilDiv(Hout, Iw)

    def dataIndex(row: Int, col: Int, channel: Int) = Index(dataGrid, row, col, channel)

    def formula3_12 = Hin * C * dataWidth // data buffer capacity / row

    def formula3_15 = blockedWh * blockedIw // PE buffer output fragment length
    def formula3_16 = 2 * Ih * Iw * blockedIw * dataWidth // PE buffer capacity

    def formula3_17 = B * Hout * dataWidth
    def formula3_18 = blockedWw * Ih * (log2Up(Hin) + log2Up(Hin) + log2Up(C))
    def formula3_19 = ceilDiv(Ih * blockedIw, blockedWh * blockedWw)
    val B = formula3_19
  }

  val res001 = ToeplitzConv(64, 3, 224, 224, 7, 7, 2, 16, 1, 3)

  // type2 bottleneck
  val res2_1_a = ToeplitzConv(64, 64, 56, 56, 1, 1, 1, 6, 1, 1)
  val res2_1_b = ToeplitzConv(64, 64, 56, 56, 3, 3, 1, 16, 1, 3)
  val res2_1_c = ToeplitzConv(256, 64, 56, 56, 1, 1, 1, 7, 1, 3)
  val res2_1_branch = ToeplitzConv(256, 64, 56, 56, 1, 1, 1, 7, 1, 3)

  // type1 bottleneck
  val res2_2_a = ToeplitzConv(256, 64, 56, 56, 1, 1, 1, 3, 1, 7)
  val res2_2_b = ToeplitzConv(64, 64, 56, 56, 3, 3, 1, 16, 1, 3)
  val res2_2_c = ToeplitzConv(64, 256, 56, 56, 1, 1, 1, 7, 1, 3)

  val res2_3_a = ToeplitzConv(256, 64, 56, 56, 1, 1, 1, 3, 1, 7)
  val res2_3_b = ToeplitzConv(64, 64, 56, 56, 3, 3, 1, 16, 1, 3)
  val res2_3_c = ToeplitzConv(64, 256, 56, 56, 1, 1, 1, 7, 1, 3)

  val res3_1_a = ToeplitzConv(128, 256, 56, 56, 1, 1, 2, 11, 1, 1)
  val res3_1_b = ToeplitzConv(128, 128, 28, 28, 3, 3, 1, 12, 1, 4)
  val res3_1_c = ToeplitzConv(128, 512, 28, 28, 1, 1, 1, 21, 1, 1)
  val res3_1_branch = ToeplitzConv(512, 256, 56, 56, 1, 1, 2, 21, 2, 1)

  // type2 bottleneck
  val res3_2_a = ToeplitzConv(128, 512, 28, 28, 1, 1, 1, 3, 1, 7)
  val res3_2_b = ToeplitzConv(128, 128, 28, 28, 3, 3, 1, 12, 1, 4)
  val res3_2_c = ToeplitzConv(512, 128, 28, 28, 1, 1, 1, 21, 1, 1)

  val res3_3_a = ToeplitzConv(128, 512, 28, 28, 1, 1, 1, 3, 1, 7)
  val res3_3_b = ToeplitzConv(128, 128, 28, 28, 3, 3, 1, 12, 1, 4)
  val res3_3_c = ToeplitzConv(512, 128, 28, 28, 1, 1, 1, 21, 1, 1)
}

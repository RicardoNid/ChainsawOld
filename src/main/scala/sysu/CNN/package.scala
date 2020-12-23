package sysu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import breeze.linalg._
import breeze.numerics._
import breeze.signal._

package object CNN {

  import sysu.xilinx.{VivadoFlowOld => VivadoFlow}

  def div(a: Int, b: Int) = ceil(a.toDouble / b.toDouble).toInt

  // generate 2D inputMaps
  def inputMaps(C: Int, H: Int, W: Int, K: Int,
                dealPadding: Boolean = true // while true, padding with 0
               ) = {
    require(K % 2 == 1, "kernel size should be odd")
    val P = K / 2
    (0 until C).map(i => DenseMatrix.tabulate(H + 2 * P, W + 2 * P) { case (j, k) => {
      val h = j - P
      val w = k - P
      if (dealPadding) if (h >= 0 && h < H && w >= 0 && w < W) i * H * W + h * H + w + 1 else 0
      else i * H * W + j * H + k + 1
    }
    })
  }

  // convert a 2D inputMap to relaxed Toeplitz Matrix
  def toeplize2D(inputMap: DenseMatrix[Int], K: Int) = {
    println("input")
    println(inputMap)
    val hOut = inputMap.rows - K + 1
    val toeplized = DenseMatrix.zeros[Int](K * K, hOut * hOut)
    for (i <- 0 until hOut; j <- 0 until hOut) {
      val temp = inputMap(i until (i + K), j until (j + K)).t
      toeplized(::, i * hOut + j) := DenseVector(temp.toArray)
    }
    toeplized
  }

  // generate weight matrix
  def weights(N: Int, C: Int, K: Int) = {
    require(K % 2 == 1, "kernel size should be odd")
    DenseMatrix.tabulate(N, C * K * K) { case (j, k) => j * C * K * K + k }
  }

  // padding at bottom-right
  def pad2D(inputMap: DenseMatrix[Int], h: Int, w: Int, mode: String) = {
    val rows = inputMap.rows
    val cols = inputMap.cols
    val padded = DenseMatrix.zeros[Int](rows + h, cols + w)
    if (mode == "BR") padded(0 until rows, 0 until cols) := inputMap // bottom-right
    if (mode == "E") padded(h / 2 until rows - h / 2, w / 2 until cols - w / 2) := inputMap // around
    padded
  }

  def getColArrays(matrix: DenseMatrix[Int]): Seq[Array[Int]] =
    (0 until matrix.cols).map(i => matrix(::, i).toArray)

  //  def getRowArrays(matrix: DenseMatrix[Int])

  //  def seq2Col (IndexedSeq)

  // optimization : 对访存的优化通过优化访存序列实现,BRAMWizard只复杂将此序列实现
  def addrOpt(addrCols: DenseMatrix[Int]): DenseMatrix[Int] = { // 之后持续优化这个方法
    val cols = (0 until addrCols.cols).map(i => addrCols(::, i))
    cols.distinct.map(_.asDenseMatrix.t).reduce(DenseMatrix.horzcat(_, _))
  }

  // todo : 以迭代器/生成器方式实现这个方法
  // todo : 需要处理dontCare
  // todo : 需要解决heap溢出问题
  // todo : 应该接收access算子作为参数
  def access(flattenedMap: DenseMatrix[Int], h: Int, w: Int) = {
    val rows = flattenedMap.rows
    val cols = flattenedMap.cols
    val padded = pad2D(flattenedMap, h, w, "BR") // in case of array index out of range
    println("toeplitz")
    println(padded)
    val seqVec = for (i <- 0 until rows by h;
                      j <- 0 until cols by w) yield {
      val temp = padded(i until (i + h), j until (j + w)).t
      //      new DenseMatrix(h * w, 1, temp.toArray)
      DenseMatrix(temp.toArray)
    }
    //    seqVec.reduce(DenseMatrix.horzcat(_, _))
    val addrSeq = seqVec.reduce(DenseMatrix.vertcat(_, _))
    addrOpt(addrSeq)
  }

  def accessMa(inputMap: DenseMatrix[Int], H: Int, W: Int, K: Int, pox: Int, poy: Int, Stride: Int = 1) = {
    // fixme : 暂未考虑不能整除的问题
    val hOut = H
    val wOut = W
    // design : by 很好用
    val tempSeq: Seq[DenseMatrix[Int]] = for (i <- 0 until H by poy; j <- 0 until wOut by pox; ky <- 0 until K by 1; kx <- 0 until K by 1) yield {
      val temp = for (oy <- 0 until poy by Stride; ox <- 0 until pox by Stride) yield inputMap(i + ky + oy, j + kx + ox)
      DenseMatrix(temp)
    }
    tempSeq.reduce(DenseMatrix.vertcat(_, _))
  }

  def addrSeq(C: Int, H: Int, W: Int, K: Int, I_h: Int, I_w: Int, print: Boolean = false) = {
    val valueSeqs = access(inputMaps(C, H, W, K).map(toeplize2D(_, K)).reduce(DenseMatrix.vertcat(_, _)), I_h, I_w)

    if (print) println("input addr\n" + valueSeqs)
    valueSeqs
  }

  def addrSeqMa(C: Int, H: Int, W: Int, K: Int, pox: Int, poy: Int, print: Boolean = false) = {
    val valueSeqs = accessMa(inputMaps(C, H, W, K)(0), H, W, K, pox, poy, 1)
    if (print) println("input addr\n" + valueSeqs)
    valueSeqs
  }

  def inputMapRowAddrSeq(C: Int, H: Int, W: Int, K: Int, I_h: Int, I_w: Int, print: Boolean = false) = {
    val hOut = H // fixme : 不够泛用
    val toeplizedMaps = inputMaps(C, H, W, K).map(toeplize2D(_, K))
    val toeplizedRows = toeplizedMaps.map(_ (::, 0 until hOut))

    val addrSeqs = access(toeplizedRows.reduce(DenseMatrix.vertcat(_, _)), I_h, I_w)
    if (print) println("input addr\n" + addrSeqs)
    addrSeqs
  }

  def weightAddrSeq(N: Int, C: Int, K: Int, W_h: Int, W_w: Int, print: Boolean = false) = {
    val addrSeqs = access(weights(N, C, K), W_h, W_w)
    if (print) println("weight addr\n" + addrSeqs)
    addrSeqs
  }
}

import breeze.linalg._
import breeze.numerics._

package object CNN {

  def div(a: Int, b: Int) = ceil(a.toDouble / b.toDouble).toInt

  // generate 2D inputMaps
  def inputMaps(C: Int, H: Int, W: Int, K: Int, dealPadding: Boolean = false) = {
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

  def weights(N: Int, C: Int, K: Int) = {
    require(K % 2 == 1, "kernel size should be odd")
    DenseMatrix.tabulate(N, C * K * K) { case (j, k) => j * C * K * K + k }
  }

  // convert a 2D inputMap to relaxed Toeplitz Matrix
  def toeplize2D(inputMap: DenseMatrix[Int], K: Int) = {
    val hOut = inputMap.rows - K + 1
    val toeplized = DenseMatrix.zeros[Int](K * K, hOut * hOut)
    for (i <- 0 until hOut; j <- 0 until hOut) {
      val temp = inputMap(i until (i + K), j until (j + K)).t
      toeplized(::, i * hOut + j) := DenseVector(temp.toArray)
    }
    toeplized
  }

  // padding at bottom-right
  def pad2DBR(inputMap: DenseMatrix[Int], h: Int, w: Int) = {
    val rows = inputMap.rows
    val cols = inputMap.cols
    val padded = DenseMatrix.zeros[Int](rows + h, cols + w)
    padded(0 until rows, 0 until cols) := inputMap
    padded
  }

  // todo : 以迭代器/生成器方式实现这个方法
  // todo : 需要处理dontCare
  // todo : 需要解决heap溢出问题
  // todo : 应该接收access算子作为参数
  def access(toeplized: DenseMatrix[Int], h: Int, w: Int) = {
    val rows = toeplized.rows
    val cols = toeplized.cols
    val padded = pad2DBR(toeplized, h, w)
    println("toeplitz")
    println(padded)
    val seqVec = for (i <- 0 until rows by h;
                      j <- 0 until cols by w) yield {
      val temp = padded(i until (i + h), j until (j + w)).t
      //      new DenseMatrix(h * w, 1, temp.toArray)
      DenseMatrix(temp.toArray)
    }
    //    seqVec.reduce(DenseMatrix.horzcat(_, _))
    seqVec.reduce(DenseMatrix.vertcat(_, _))
  }

  def inputMapAddrSeq(C: Int, H: Int, W: Int, K: Int, I_h: Int, I_w: Int, print: Boolean = false) = {
    val addrSeqs = access(inputMaps(C, H, W, K).map(toeplize2D(_, K)).reduce(DenseMatrix.vertcat(_, _)), I_h, I_w)
    if (print) println("input addr\n" + addrSeqs)
    addrSeqs
  }

  def weightAddrSeq(N: Int, C: Int, K: Int, W_h: Int, W_w: Int, print: Boolean = false) = {
    val addrSeqs = access(weights(N, C, K), W_h, W_w)
    if (print) println("weight addr\n" + addrSeqs)
    addrSeqs
  }

  def addrOpt(addrCols:DenseMatrix[Int]) : DenseMatrix[Int]={
    val cols = (0 until addrCols.cols).map(i => addrCols(::, i))
    
  }
}

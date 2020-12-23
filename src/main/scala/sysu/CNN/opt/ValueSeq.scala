package sysu.CNN.opt

import breeze.linalg._
import breeze.numerics._
import breeze.signal._

import scala.collection.mutable.ListBuffer

// 0 means padding, index starts from 1

class ValueSeq(original: matrix_t) {

  var data = original

  def col(i: Int) = data(::, i)

  def row(i: Int) = data(i, ::).t

  // optimization : 是否可以不遍历?
  def getSimilarity(a: vector_t, b: vector_t) = {
    require(a.length == b.length)
    val arrayA = a.toArray
    val arrayB = b.toArray
    val length = arrayA.length
    val lengthNoPadding = arrayA.filter(_ != 0).length
    var shift = 0
    var maxSimilarity = 0.0
    // 向上移位寻找
    val similarities0 = (0 until length).map(i => {
      val similarity = arrayA.drop(i)
        .zip(arrayB)
        .map(tuple => if (tuple._1 == tuple._2 && tuple._1 != 0) 1 else 0)
        .reduce(_ + _) / lengthNoPadding.toDouble
      if (similarity > maxSimilarity) {
        maxSimilarity = similarity
        shift = i
      }
    })
    // 向下移位寻找
    val similarities1 = (0 until length).map(i => {
      val similarity = arrayA.reverse.drop(i)
        .zip(arrayB.reverse)
        .map(tuple => if (tuple._1 == tuple._2 && tuple._1 != 0) 1 else 0)
        .reduce(_ + _) / lengthNoPadding.toDouble
      if (similarity > maxSimilarity) {
        maxSimilarity = similarity
        shift = -i
      }
    })
    (maxSimilarity, shift)
  }

  def keepUnique(array: Array[Int]) = {
    (0 until array.length).map(i => {
      val ele = array(i)
      if (ele == 0) 0 else {
        val prePos = array.indexOf(ele)
        if (prePos != i) prePos - i
        else array(i)
      }
    }).toArray
  }

  def getFIFOMap = {
    val h = data.rows
    val w = data.cols
    val FIFOMap = Array.ofDim[Tuple2[Int, Int]](h, w)
    for (i <- 0 until h; j <- 0 until w) {
      val index = i * w + j
      val ele = data(i, j)
      FIFOMap(i)(j) = if (ele < 0) {
        val port = (index + ele) % w
        val delay = -ele / w + 1
        (port, delay)
      } else (0, 0)
    }
    FIFOMap
  }

  def printArray2D[T](array: Array[Array[T]]): Unit = {
    //
  }

  def rowUnique = (0 until data.rows).foreach(i => row(i) := DenseVector(keepUnique(row(i).toArray)))

  def matrixUnique = {
    val h = data.rows
    val w = data.cols
    data = new DenseMatrix(w, h, keepUnique(data.t.toArray)).t
  }

  def bandWidthBound = (0 until data.rows).map(row(_).toArray.filter(_ != 0).distinct.length).max

  def getColSimilarity(a: Int, b: Int) = getSimilarity(col(a), col(b))

  def colInterchange(i: Int, j: Int): Unit = {
    val temp = col(i).copy
    col(i) := col(j)
    col(j) := temp
  }

  def colShift(colIndex: Int, shift: Int) = {
    val target = col(colIndex)
    val shifted = if (shift > 0) target.toArray.drop(shift).padTo(target.length, 0)
    else target.toArray.reverse.drop(-shift).padTo(target.length, 0).reverse
    target := DenseVector(shifted)
  }

  //
  //  def dropEmptyCol

  def opt() = {
    for (i <- 0 until data.cols - 1) {
      var cand = i + 1
      for (j <- i + 1 until data.cols) {
        val (similarity, shift) = getColSimilarity(i, j)
        println(similarity)
        if (similarity > 0.5) {
          colInterchange(j, cand) // todo : 副作用-switch
          if (shift > 0) colShift(i, shift) // todo : 副作用-fifo
          else colShift(j, -shift)
          cand += 1
        }
      }
    }
  }
}

object ValueSeq {
  def main(args: Array[String]): Unit = {
    import sysu.CNN._
//    val seq = addrSeq(3, 7, 7, 3, 4, 4, true)
        val seq = addrSeqMa(1, 6, 6, 3, 3, 3, true)
    val demo = new ValueSeq(seq)
    //    demo.opt()
    demo.matrixUnique
    //    demo.rowUnique
    println(demo.data)
    println(demo.bandWidthBound)
    println(demo.getFIFOMap.map(_.mkString(" ")).mkString("\n"))
    println(demo.getFIFOMap.flatten.distinct.mkString(" "))
    println(demo.getFIFOMap.flatten.distinct.length)
  }
}



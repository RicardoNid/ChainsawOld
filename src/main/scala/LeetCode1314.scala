import scala.collection.mutable

class LeetCode1314 {

  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {

    var resTable = mutable.Map[Tuple3[Int, Int, Int], Int]()
    var crossTable = mutable.Map[Tuple3[Int, Int, Int], Int]()

    val m = mat.length
    val n = mat(0).length

    for (k <- 0 to K) {
      for (i <- 0 until m; j <- 0 until n) {
        if (k == 0) {
          resTable += ((i, j, k) -> mat(i)(j))
          crossTable += ((i, j, k) -> mat(i)(j))
        }
        else {
          // update crossTable
          val cross = crossTable(i, j, k - 1)
          +resTable.getOrElse((i - k, j, 0), 0)
          +resTable.getOrElse((i + k, j, 0), 0)
          +resTable.getOrElse((i, j + k, 0), 0)
          +resTable.getOrElse((i, j - k, 0), 0)
          crossTable += (i, j, k) -> cross
          // update resTable
          val prev = if (k % 2 == 0) (k / 2) else ((k - 1) / 2)
          val res = resTable.getOrElse((i - 1, j - 1, prev), 0)
          +resTable.getOrElse((i + 1, j - 1, prev), 0)
          +resTable.getOrElse((i - 1, j + 1, prev), 0)
          +resTable.getOrElse((i + 1, j + 1, prev), 0)
          +crossTable((i, j, k))
          resTable += (i, j, k) -> res
        }
      }
    }

    Array.tabulate(m, n)(resTable(_, _, K))
  }
}

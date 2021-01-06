package projects

package object Wht {

  val ori = Array.tabulate(32, 16)(_ * 16 + _)
  val map1 = (i: Int) => if (i > 15 && (i % 16 != 0)) i - 17 else -1
  val map2 = (i: Int) => if (i > 15) i - 16 else -1
  val map3 = (i: Int) => if (i % 16 != 0) i - 1 else -1
  // 原始数据及其依赖项
  val source = ori.map(_.map(i => Array(i, map1(i), map2(i), map3(i)))).flatten
  println(source.map(_.mkString(" ")).mkString("\n"))
  // 原始数据的实际顺序
  val reorder = ori.flatten.sortBy(i => ((i / 16) + (i % 16)) * 16 + (i / 16))
  // 按顺序重排的映射
  val remap = reorder.zipWithIndex.map {
    case (ele, i) => ele -> i
  }.toMap

  val stream = source.map(_.map(i => if (i >= 0) remap(i) else -1)).sortBy(_ (0))
  val switch = stream.map(_.map(i => if (i >= 0) 1 else 0))
  val reuse = stream.zip(switch).map {
    case (row1, row2) => {
      Array(
        (row1(0) - row1(1)) * row2(1),
        (row1(0) - row1(2)) * row2(2),
        (row1(0) - row1(3)) * row2(3))
    }
  }
  reuse.map(_.count(_ == 31)).sum

  val seqs = (0 until 3).map(i => reuse.map(_ (i)).toArray)

  println(seqs(1).mkString("\n"))

}

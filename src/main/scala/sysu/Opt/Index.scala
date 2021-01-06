package sysu.Opt

/*
  @param : grid 所在空间的尺寸
  @param : coords 坐标
 */

case class Index(grid: Array[Int], coords: Int*) {
  val dimension = coords.length
  require(coords.forall(_ >= -1))
  require(coords.length <= 4)
  require(coords.length == grid.length)

  def coord1D = (0 until dimension - 1).map(i => coords(i) * grid.drop(i + 1).reduce(_ * _)).reduce(_ + _) + coords.last

  override def toString: String = {
    val content = coords.length match {
      case 1 => coords(0).toString
      case 2 => coords(0).toString + " " + coords(1).toString
      case 3 => coords(0).toString + " " + coords(1).toString + " " + coords(2).toString
      case 4 => coords(0).toString + " " + coords(1).toString + " " + coords(2).toString + " " + coords(3).toString
    }
    "(" + content + ")"
  }
}

object Index {
  def main(args: Array[String]): Unit = {
    val grid1 = Array(3, 3, 3, 3)
    val index1 = Index(grid1, 1, 2, 3, 2)
    println(index1.coord1D)
  }
}



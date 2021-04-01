package sysu



package object util {

  def format(s: String, width: Int) = s.padTo(width, " ").mkString("")

  def printArray2D[T](array: Array[Array[T]], width: Int = 5, head: Boolean = true, tail: Boolean = false) = {
    if (tail) println(array.drop(array.length - 100).map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
    else if (head) println(array.dropRight(array.length - 100).map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
    else println(array.map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
  }
}

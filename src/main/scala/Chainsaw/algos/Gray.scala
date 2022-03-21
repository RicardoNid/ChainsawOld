package Chainsaw.algos

/** conversions between binary and gray code
  */
object Gray {

  def grayTable(width: Int): Seq[Int] = {

    // this is how gray code grow
    def grow(data: Seq[Int]) = data ++ data.reverse.map(_ + data.length)

    if (width == 1) Seq(0, 1) // initial
    else grow(grayTable(width - 1)) // grow
  }

  def toGray(bin: Int, width: Int) = grayTable(width)(bin)

  def fromGray(gray: Int, width: Int) = grayTable(width).indexOf(gray)

  def main(args: Array[String]): Unit = {
    val bits  = 6
    val grays = (0 until (1 << bits)).map(toGray(_, bits))
    val bins  = grays.map(fromGray(_, bits))

    println(grays)
    println(bins)
  }
}

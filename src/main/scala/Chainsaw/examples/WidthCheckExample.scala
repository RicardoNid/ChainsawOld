package Chainsaw.examples

object WidthCheckExample {
  def main(args: Array[String]): Unit = {

    def isNBit(value: Int, n: Int) = value >= -(1 << (n - 1)) && value < (1 << (n - 1)) - 1

    val isShort: Int => Boolean = isNBit(_, 16)
    val isChar: Int => Boolean  = isNBit(_, 8)

    def work(value: Int): Unit = {
      require(isChar(value), s"value should be of unsigned 8-bits while it is $value")
    }

    work(311)
  }
}

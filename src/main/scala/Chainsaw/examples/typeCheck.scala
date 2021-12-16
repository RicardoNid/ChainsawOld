package Chainsaw.examples

object typeCheck {
  def main(args: Array[String]): Unit = {
    val typeMap = Map(Int -> BigInt, Double -> BigDecimal)

    def convert[THard, TSoft](hard: THard, soft: TSoft) = {
      hard match {
        case int: Int =>
          val softType = typeMap(Int)
        case double: Double => double.asInstanceOf[BigDecimal]
      }
    }

//    println(convert(1))
  }
}

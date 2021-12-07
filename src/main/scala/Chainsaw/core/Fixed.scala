package Chainsaw.core

import scala.Predef

class Fixed(val value: Int, val signed: Boolean, val wordLength: Int, val fractionLength: Int) {

  def toDouble = value / (1 << fractionLength).toDouble

  def >(that: Fixed) = value > that.value

  def <(that: Fixed) = that > this

  def ==(that: Fixed) = value == that.value
}

object FixedNumeric extends Numeric[Fixed] {

  override def plus(x: Fixed, y: Fixed): Fixed = Fixed(x.value + y.value, x.signed, x.wordLength, x.fractionLength)

  override def minus(x: Fixed, y: Fixed): Fixed = Fixed(x.value + y.value, x.signed, x.wordLength, x.fractionLength)

  override def times(x: Fixed, y: Fixed): Fixed = Fixed(x.value + y.value, x.signed, x.wordLength, x.fractionLength)

  override def negate(x: Fixed): Fixed = Fixed(x.value, x.signed, x.wordLength, x.fractionLength)

  override def fromInt(x: Int): Fixed = Fixed(x, true, 1, 1)

  override def toInt(x: Fixed): Int = x.value

  override def toLong(x: Fixed): Long = x.value

  override def toFloat(x: Fixed): Float = x.value

  override def toDouble(x: Fixed): Double = x.value

  override def compare(x: Fixed, y: Fixed): Int = if (x > y) 1 else if (x < y) -1 else 0
}

object Fixed {

  def apply(value: Int, signed: Boolean, wordLength: Int, fractionLength: Int): Fixed = new Fixed(value, signed, wordLength, fractionLength)

  def main(args: Array[String]): Unit = { // simple tests
    val fi2 = Fixed(10, true, 5, 2)
    val fi3 = Fixed(11, true, 5, 2)
    implicit val fixedNumeric = FixedNumeric

    println(Seq(fi2, fi3).sorted.mkString(" "))


  }
}

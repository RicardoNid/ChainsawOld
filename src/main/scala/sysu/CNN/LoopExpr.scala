package sysu.CNN

import scala.collection.mutable.Map

case class CounterParam(
                       T:Int = 100,
                       S:Int = 3,
                       W:Int = 3
                       )

case class LoopExpr(name:String) {

  // operation order = % /
  val variable : Map[String,Array[Int]] = Map(name -> Array(100, 1, 1))
  val constant  = Array(0)

  def + (that:LoopExpr) = {
    this.variable ++= that.variable
    this
  }

  def + (number:Int) = {
    this.constant(0) += number
    this
  }

  def * (number : Int) = {
    this.variable(name)(1) *= number
    this
  }

  def % (number : Int) = {
    this.variable(name)(0) = number
    this
  }

  def / (number: Int) = {
    this.variable(name)(2) *= number
    this
  }
}

object LoopExpr {
  implicit class Var(val sc : StringContext) extends AnyVal{
    def v(args: Any*) = new LoopExpr(sc.parts(0))
  }

  def main(args: Array[String]): Unit = {
    val expr = v"kx" % 3 + v"ky" / 5 + 10 + 30
    println(expr.variable.values.map(_.mkString(" ")).mkString("\n"))
    println(expr.constant(0))
  }
}





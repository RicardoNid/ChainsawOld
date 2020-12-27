import scala.collection.mutable.Map

// todo : 參考State的設計,設計用於LoopNest的Var類型
// 逐漸發展成一種"LoopNest DSA"

trait LoopVar {
  val name = ""
  val report: Map[String, Array[Int]] = Map()

  def +(that: LoopVar) = {
    this.report ++= that.report
    this
  }

  def %(number: Int) = {
    report(name)(0) = number
    this
  }

  def /(number: Int) = {
    report(name)(1) = number
    this
  }
}

case object OY extends LoopVar {
  override val name = "oy"
  override val report = Map("oy" -> Array(0, 0))
}

case object OX extends LoopVar {
  override val name = "ox"
  override val report = Map("ox" -> Array(0, 0))
}

case object OF extends LoopVar // n
case object IF extends LoopVar // c
case object KY extends LoopVar // ky
case object KX extends LoopVar // kx

(OX + OY / 5).report
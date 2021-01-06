package sysu.Opt

import spinal.core._
import spinal.core.sim._
import sysu.util._

import scala.collection.mutable.{ArrayBuffer, Map}

case class CounterParam(
                         var period: Int,
                         var step: Int,
                         var width: Int
                       )

// todo 以隐式参数的方式处理初始period
case class LoopExpr(var name: String) {
  // operation order = % /
  val variable: Map[String, CounterParam] = Map(name -> CounterParam(100, 1, 1))
  var constant = 0

  def +(that: LoopExpr) = {
    this.variable ++= that.variable
    this.constant += that.constant
    this
  }

  def +(number: Int) = {
    this.constant += number
    this
  }

  def *(number: Int) = {
    this.variable.values.foreach(_.step *= number)
    this
  }

  def /(number: Int) = {

    this.variable.values.foreach { param =>
      val s = param.step
      require(s % number == 0 || number % s == 0)
      if (s >= number) param.step /= number
      else {
        param.step = 1
        param.width *= (number / s)
      }
    }
    this
  }

  def %(number: Int) = {
    val vanish = ArrayBuffer[String]()
    this.variable.foreach { case (name, param) =>
      val s = param.step
      require(s % number == 0 || number % s == 0)
      if (s >= number) vanish.append(name)
      else param.period = number / s
    }
    vanish.foreach(name => this.variable -= name)
    this
  }

  def mod(number: Int)(name: String) = {
    val s = this.variable(name).step
    require(s % number == 0 || number % s == 0)
    if (s >= number) this.variable -= name
    else this.variable(name).period = number / s
    this
  }

  def commonPeriod = variable.map(_._2.period).max

  // todo 使用lazy方法
  def softSeqGen = (0 until commonPeriod).map { i =>
    variable.values.map { param =>
      i % param.period / param.width * param.step
    }.reduce(_ + _) + constant
  }

  def max = softSeqGen.max

  def hardSeqGen = {
    new LoopExprSeqGen(this)
  }

  def verify = {
    val sw = softSeqGen
    val period = 2
    SimConfig.compile(hardSeqGen)
      .doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectorssw
          sleep(17 * period)
          for (i <- 0 until commonPeriod) {
            assert(sw(i) == dut.io.output.toInt, s"${sw(i)} & ${dut.io.output.toInt}")
            sleep(period)
          }
          simSuccess()
        }
      }
  }
}

object LoopExpr {

  implicit class Var(val sc: StringContext) extends AnyVal {
    def v(args: Any*) = new LoopExpr(sc.parts(0))
  }

  def traverse(loopExpr: LoopExpr) = {
    loopExpr.variable("ox").width *= 1
    loopExpr.softSeqGen
  }

  def main(args: Array[String]): Unit = {
    val expr0 = v"ox" % 3 + v"oy" * 3 % 3 + 3
    expr0.mod(2)("ox")
    expr0.variable.values.foreach(println)
    println("constant : " + expr0.constant)

    expr0.verify

    //    val report = VivadoFlow(design = expr0.hardSeqGen, vivadoConfig = recommended.vivadoConfig,
    //      vivadoTask = VivadoTask(
    //        topModuleName = "expr",
    //        workspacePath = "output/expr"
    //      ),
    //      force = true).doit()
    //    report.printArea
    //    report.printFMax

    traverse(expr0) foreach println
  }
}





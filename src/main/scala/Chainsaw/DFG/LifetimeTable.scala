package Chainsaw.DFG

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// including end but not start
class Lifetime(var start: Int, var end: Int, val name: String = "unnamed") {
  def liveAt(time: Int) = ((start + 1) to end).contains(time)

  def length = end - start

  def dead = end <= start

  override def toString: String = s"${name.padTo(5, ' ')}: $start -> $end"
}

object Lifetime {
  def apply(start: Int, end: Int, name: String): Lifetime = new Lifetime(start, end, name)
}

case class RegTrace(path: ArrayBuffer[Int], name: String = "unnamed") {
  def +=(next: Int) = path += next

  override def toString: String = s"$name: ${path.mkString(" -> ")}"
}

class LifetimeTable {

  val lifetimes = ArrayBuffer[Lifetime]()
  var period = 1

  def range = {
    val start = lifetimes.map(_.start).min
    val end = lifetimes.map(_.end).max
    (start + 1) to end
  }

  def rangeForAnalysis = {
    val start = (range.size / period) * period
    val end = start + period
    (start + 1) to end
  }

  def lifetimesForAnalysis = {
    val ret = ArrayBuffer[Lifetime]()
    lifetimes.foreach { lifetime =>
      var start = lifetime.start
      var times = 0
      while (start + 1 <= rangeForAnalysis.end) {
        val end = start + lifetime.length
        if (end >= rangeForAnalysis.start) ret += Lifetime(start, end, lifetime.name + "'" * times)
        start += period
        times += 1
      }
    }
    ret
  }

  def setPeriod(value: Int) = period = value

  def addVariable(lifetime: Lifetime): Unit = lifetimes += lifetime

  def getMinimumRegCount = { // TODO: better algorithm
    println(lifetimesForAnalysis.mkString("\n"))
    rangeForAnalysis.map(time => lifetimesForAnalysis.filter(_.liveAt(time)).size).max
  }

  def getAllocation = {

    val regCount = getMinimumRegCount
    val space = Array.tabulate(period, regCount)((_, _) => "") // init the space which is empty
    val traces = lifetimes.map(lifetime => lifetime -> RegTrace(ArrayBuffer[Int](), lifetime.name)).toMap

    // step1, input
    (1 to period).foreach { time =>
      val candidates = lifetimes.filter(_.start + 1 == time).sortBy(_.length).reverse // big -> small
      candidates.zipWithIndex.foreach { case (lifetime, i) =>
        space(time - 1)(i) = lifetime.name
        traces(lifetime) += i
      }
    }
    println(space.map(_.map(_.padTo(4, ' ')).mkString(" ")).mkString("\n"))
    lifetimes.foreach(_.start += 1)

    // step2, forward allocation & periodic behavior
    lifetimes.sortBy(_.length).reverse.sortBy(_.start).foreach { lifetime =>
      println(s"forwarding ${lifetime.name}")
      var succeed = true
      while (!lifetime.dead && succeed) {
        val currentLocation = traces(lifetime).path.last
        val row = lifetime.start % period
        val col = space(row).drop(currentLocation + 1).indexWhere(_.isEmpty) // first empty space
        if (col >= 0) {
          space(row)(col + currentLocation + 1) = lifetime.name
          lifetime.start += 1
          traces(lifetime) += (col + currentLocation + 1)
        }
        else succeed = false
      }
    }
    println("after forward allocation")
    println(space.map(_.map(_.padTo(4, ' ')).mkString(" ")).mkString("\n"))



    (space, traces.values)
  }

  def impl {}

}

object LifetimeTable {
  def apply(): LifetimeTable = new LifetimeTable()
}

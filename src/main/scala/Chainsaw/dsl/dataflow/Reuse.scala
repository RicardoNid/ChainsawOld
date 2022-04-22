package Chainsaw.dsl.dataflow

import breeze.numerics.ceil


case class Reuse(parallel: Int, iterative: Int, latency: Int) {

  def factors(value: Int) = (1 to value).filter(value % _ == 0)

  def adapt(throughput: Double) = {

    var currentThroughput = 1.0

    // find reuse in parallel repetition
    val actualParallel = factors(parallel).filter(_.toDouble / parallel * currentThroughput >= throughput).head

    currentThroughput *= (actualParallel.toDouble / parallel)
    println(currentThroughput)

    val actualIterative = factors(iterative).filter {
      newIterative =>
        val newThroughput =
          if (newIterative == iterative) currentThroughput
          else {
            val period = newIterative * latency
            val utilization = actualParallel.toDouble / period max 1.0
            utilization * newIterative / iterative * currentThroughput
          }
        newThroughput >= throughput
    }.head

    currentThroughput *= (actualIterative.toDouble / iterative)

    println(actualParallel, actualIterative, currentThroughput)
  }

}

object Reuse {
  def main(args: Array[String]): Unit = {
    val example = Reuse(4,4,1).adapt(0.125)
  }
}

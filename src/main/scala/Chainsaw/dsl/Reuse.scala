package Chainsaw.dsl


case class Reuse(spaceReuse: Int, timeReuse: Int, fold: Int, iterationLatency:Int)

object Reuse {

  def findReuse(targetThroughput: Double, repetition: Repetition, impl: Impl): Reuse = {

    import repetition.{spaceFactor, timeFactor}
    import impl.foldMax

    var spaceReuse = 1
    var timeReuse = 1
    var fold: Int = 1

    def latency = impl.getImpl(fold).latency

    def iterationLatency = (timeFactor / timeReuse * latency) max (spaceReuse * fold)

    def throughput = 1.0 / (timeReuse * iterationLatency)

    def util = spaceReuse * fold / iterationLatency

    def pass = throughput >= targetThroughput

    // find reuse in space repetition
    factors(spaceFactor).foreach { reuse =>
      val old = spaceReuse
      spaceReuse = reuse
      if (!pass) spaceReuse = old
    }
    // find reuse in folding
    factors(foldMax).foreach { reuse =>
      val old = fold
      fold = reuse
      if (!pass) fold = old
    }
    // find reuse in time
    factors(timeFactor).foreach { reuse =>
      val old = timeReuse
      timeReuse = reuse
      if (!pass) timeReuse = old
    }

    Reuse(spaceReuse, timeReuse, fold, iterationLatency)
  }
}

package Chainsaw.comm.viterbi

import Chainsaw._
import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, Queue, Stack}

object Algos {

  /** General Viterbi Algo without traceback(save the paths)
    *
    * @param observed
    *   observed sequence of output symbols with noise
    * @param trellis
    *   trellis with input symbol type: Int and output symbol type: T
    * @param metric
    *   metric that defines the difference between an expected output symbol and an observed output symbol
    * @tparam T
    *   type of output symbols, could be int(hard) or double(soft)
    */
  def viterbi[T](observed: Array[T], trellis: Trellis[T], metric: (T, T) => Double): Array[Int] = {

    import trellis._

    case class Path(states: Seq[Int], discrepancy: Double) {
      def head = states.head

      override def toString() = states.mkString(" -> ") + s" $discrepancy"
    }

    // initialize the paths
    val observedQueue = Queue(observed: _*)
    var paths         = Path(Queue(0), 0) +: (1 until trellis.numStates).map(i => Path(Queue(i), Int.MaxValue)) // paths(i) always ends at state i
    val decoded       = ArrayBuffer[Int]()

    while (observedQueue.nonEmpty) {
      // compare, select, update
      val currentObserved = observedQueue.dequeue()
      paths = (0 until numStates).map { currentState =>
        val prevData          = lookBackMap(currentState)
        val prevPaths         = prevData.map(_._1).map(paths(_))
        val prevOutputSymbols = prevData.map(_._2)

        val survivior = prevPaths
          .zip(prevOutputSymbols)
          .map { case (path, output) =>
            val increment   = metric(output, currentObserved)
            val discrepancy = path.discrepancy + increment
            (path, discrepancy)
          }
          .minBy(_._2)

        Path(survivior._1.states :+ currentState, survivior._2)
      }
      // converge
      while (paths.forall(_.head == paths.head.head)) {
        decoded += paths.head.head
        paths = paths.map(path => Path(path.states.tail, path.discrepancy))
      }
    }
    (decoded ++ paths(0).states).toArray
  }

  /** General Viterbi Algo with traceback(write discrepancy)
    */
  def viterbiTraceback[T](observed: Array[T], trellis: Trellis[T], metric: (T, T) => Double, stateStart: Int = 0) = {

    import trellis._

    // recording discrepancies
    val records = Seq.fill(numStates)(Stack[Double]())

    // initialization
    val observedQueue = Queue(observed: _*)
    records.indices.foreach { currentState =>
      if (currentState == stateStart) records(currentState).push(0)
      else records(currentState).push(Int.MaxValue)
    }

    // writing records iteratively
    while (observedQueue.nonEmpty) {
      val currentObserved = observedQueue.dequeue()
      val updates = records.indices.map { currentState =>
        val prevData          = lookBackMap(currentState)
        val prevOutputSymbols = prevData.map(_._2)
        val prevDiscrepancies = prevData.map(_._1).map(records(_).head)
        val increments        = prevOutputSymbols.map(metric(_, currentObserved))
        val update            = prevDiscrepancies.zip(increments).map { case (dis, inc) => dis + inc }.min
        //        println(s"currentState $currentState, prev states ${prevData.map(_._1).mkString(" ")} prev symbols ${prevOutputSymbols.mkString(" ")}, " +
        //          s"prev dis ${prevDiscrepancies.mkString(" ")} prev incs ${increments.mkString(" ")} update $update")
        update
      }
      records.zip(updates).foreach { case (doubles, d) => doubles.push(d) }
    }

    // tracing back the records
    // starts from minimum
    val decoded       = ArrayBuffer[Int]()
    val discrepancies = records.map(_.pop())
    var currentState  = discrepancies.indexOf(discrepancies.min)
    decoded += currentState
    // iteratively tracing back
    val observedQueueAnother = Queue(observed: _*)
    while (records.head.nonEmpty) {
      val currentObserved = observedQueueAnother.dequeue()
      val prevData        = lookBackMap(currentState)
      val discrepancies   = records.map(_.pop())
      //      val prevState = prevData.minBy { case (state, output) => discrepancies(state) + metric(output, currentObserved) }._1
      val prevState = prevData.minBy { case (state, output) => discrepancies(state) }._1 // TODO: explain why this logic(not the line above)
      currentState = prevState
      decoded += currentState
    }

    (decoded.reverse).toArray
  }

  /** Parallel viterbi implemented by Minplus algebra
    *
    * @param P
    *   parallel factor
    */
  def viterbiParallel(observed: Array[Int], trellis: Trellis[Int], metric: (Int, Int) => Double, stateStart: Int = 0, P: Int) = {
    val N = observed.length
    require(N % P == 0)
    val minplusMatrices = MinplusMatrix.trellis2Minplus(trellis, metric)

    val segments: Seq[Array[Int]]                   = observed.grouped(N / P).toSeq
    val segmentsMatrices: Seq[Array[MinplusMatrix]] = segments.map(_.map(minplusMatrices(_)))
    val segmentMatricesProducts: Seq[MinplusMatrix] = segmentsMatrices.map(_.reduce(_ * _))

    val increasingPrefix = (1 until P).map(segmentMatricesProducts.take(_).reduce(_ * _))
    val decreasingPrefix = (1 until P).map(segmentMatricesProducts.takeRight(_).reduce(_ * _))

    val entrances = increasingPrefix.zip(decreasingPrefix.reverse).map { case (head, tail) => MinplusMatrix.findMid(head, tail, 0, 0) }
    val decodeds  = segments.zip(stateStart +: entrances).map { case (segment, entrance) => viterbiTraceback(segment, trellis, metric, entrance).tail }

    (stateStart +: decodeds.flatten).toArray
  }

  /** General Viterbi Algo with traceback(write discrepancy)
    */
  def viterbiTracebackMinplus(observed: Array[Int], trellis: Trellis[Int], metric: (Int, Int) => Double, stateStart: Int = 0) = {

    import trellis._
    val records = viterbiForwarding(observed, trellis, metric, stateStart)
    viterbiBackwarding(records, trellis)

  }

  def viterbiForwarding(observed: Array[Int], trellis: Trellis[Int], metric: (Int, Int) => Double, stateStart: Int = 0): Stack[Seq[Double]] = {

    import trellis._

    val records         = Stack[Seq[Double]]()
    val observedQueue   = Queue(observed: _*)
    val minplusMatrices = MinplusMatrix.trellis2Minplus(trellis, metric)

    // initialization
    val vector: Seq[Double]  = (0 until numStates).map(currentState => if (currentState == stateStart) 0.0 else MinplusMatrix.max)
    var currentDiscrepancies = MinplusMatrix(Array(vector.toArray))
    records.push(vector)

    // writing records iteratively
    while (observedQueue.nonEmpty) {
      val currentObserved = observedQueue.dequeue()
      val updatingMatrix  = minplusMatrices(currentObserved)
      currentDiscrepancies = currentDiscrepancies * updatingMatrix
      records.push(currentDiscrepancies.value.head)
    }

    records
  }

  def viterbiBackwarding(records: Stack[Seq[Double]], trellis: Trellis[Int]): Array[Int] = {

    import trellis._

    // starts from minimum
    val decoded       = ArrayBuffer[Int]()
    val discrepancies = records.pop()
    var currentState  = discrepancies.indexOf(discrepancies.min)
    decoded += currentState
    // iteratively tracing back
    while (records.nonEmpty) {
      val prevData      = lookBackMap(currentState)
      val discrepancies = records.pop()
      val prevState     = prevData.minBy { case (state, output) => discrepancies(state) }._1 // TODO: explain why this logic(not the line above)
      currentState = prevState
      decoded += currentState
    }

    (decoded.reverse).toArray
  }

  def Hamming(expected: Int, observed: Int): Double = (expected ^ observed).toBinaryString.map(_.asDigit).sum

}

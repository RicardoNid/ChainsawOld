package Chainsaw.Communication.Viterbi

import Chainsaw._
import spinal.core._

import scala.collection.mutable.{ArrayBuffer, Map, Queue, Stack}

object Algos {

  /** General Viterbi Algo without traceback(save the paths)
   *
   * @param observed observed sequence of output symbols with noise
   * @param trellis  trellis with input symbol type: Int and output symbol type: T
   * @param metric   metric that defines the difference between an expected output symbol and an observed output symbol
   * @tparam T type of output symbols, could be int(hard) or double(soft)
   */
  def viterbi[T](observed: Array[T], trellis: Trellis[T], metric: (T, T) => Double) = {

    import trellis._

    case class Path(states: Seq[Int], discrepancy: Double) {
      def head = states.head
      override def toString() = states.mkString(" -> ") + s" $discrepancy"
    }

    // initialize the paths
    val observedQueue = Queue(observed: _*)
    var paths = Path(Queue(0), 0) +: (1 until trellis.numStates).map(i => Path(Queue(i), Int.MaxValue)) // paths(i) always ends at state i
    val decoded = ArrayBuffer[Int]()

    while (observedQueue.nonEmpty) {
      // compare, select, update
      val currentObserved = observedQueue.dequeue()
      paths = (0 until numStates).map { currentState =>
        val prevData = lookBackMap(currentState)
        val prevPaths = prevData.map(_._1).map(paths(_))
        val prevOutputSymbols = prevData.map(_._2)

        val survivior = prevPaths.zip(prevOutputSymbols).map { case (path, output) =>
          val increment = metric(output, currentObserved)
          val discrepancy = path.discrepancy + increment
          (path, discrepancy)
        }.minBy(_._2)

        Path(survivior._1.states :+ currentState, survivior._2)
      }
      // converge
      while (paths.forall(_.head == paths.head.head)) {
        decoded += paths.head.head
        paths = paths.map(path => Path(path.states.tail, path.discrepancy))
      }
    }
    decoded ++ paths(0).states
  }

  /** General Viterbi Algo with traceback(write discrepancy)
   *
   */
  def viterbiTraceback[T](observed: Array[T], trellis: Trellis[T], metric: (T, T) => Double,
                          stateStart: Int = 0) = {

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
        val prevData = lookBackMap(currentState)
        val prevOutputSymbols = prevData.map(_._2)
        val prevDiscrepancies = prevData.map(_._1).map(records(_).head)
        val increments = prevOutputSymbols.map(metric(_, currentObserved))
        val update = prevDiscrepancies.zip(increments).map { case (dis, inc) => dis + inc }.min
        //        println(s"currentState $currentState, prev states ${prevData.map(_._1).mkString(" ")} prev symbols ${prevOutputSymbols.mkString(" ")}, " +
        //          s"prev dis ${prevDiscrepancies.mkString(" ")} prev incs ${increments.mkString(" ")} update $update")
        update
      }
      records.zip(updates).foreach { case (doubles, d) => doubles.push(d) }
      //      println(records.map(_.head).mkString(" "))
    }

    //    println(records.map(_.mkString("->")).mkString("\n"))

    // tracing back the records
    // starts from minimum
    val decoded = ArrayBuffer[Int]()
    val discrepancies = records.map(_.pop())
    //    var currentState = discrepancies.indexOf(discrepancies.min)
    //    decoded += currentState
    // starts from 0
    var currentState = 0
    decoded += currentState

    // iteratively tracing back
    val observedQueueAnother = Queue(observed: _*)
    while (records.head.nonEmpty) {
      val currentObserved = observedQueueAnother.dequeue()
      val prevData = lookBackMap(currentState)
      val discrepancies = records.map(_.pop())
      //      val prevState = prevData.minBy { case (state, output) => discrepancies(state) + metric(output, currentObserved) }._1
      val prevState = prevData.minBy { case (state, output) => discrepancies(state) }._1 // TODO: explain why this logic(not the line above)
      currentState = prevState
      decoded += currentState
    }

    decoded.reverse
  }

  def Hamming(expected: Int, observed: Int): Double = {
    val length = log2Up(expected) max log2Up(observed)
    expected.toBinaryString.padToLeft(length, '0').zip(observed.toBinaryString.padToLeft(length, '0')).filter { case (c, c1) => c != c1 }.length.toDouble
  }

  def main(args: Array[String]): Unit = {
    val constLen = 3
    val codeGen = Array(3, 5)
    val trellis = Trellis.poly2trellis(constLen, codeGen)
    val trellisM = Refs.poly2trellisM(constLen, codeGen)

    val inputData = ((0 until 100).map(_ => DSPRand.nextInt(trellis.numInputSymbols)) ++ Seq.fill(constLen - 1)(0)).toArray
    val codedData = Refs.convenc(inputData, trellisM)
    val testCase = codedData.grouped(2).map(_.reverse.zipWithIndex.map { case (i, i1) => i * (1 << i1) }.sum).toArray
    val golden = Refs.vitdecHard(codedData, trellisM, 6 * constLen)
    val yours = viterbi(testCase, trellis, Hamming).tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)
    val yoursTraceBack = viterbiTraceback(testCase, trellis, Hamming).tail.map(_.toBinaryString.padToLeft(constLen - 1, '0').head.asDigit)

    assert(yours.nonEmpty)
    assert(golden.mkString("") == yours.mkString(""))
    assert(golden.mkString("") == yoursTraceBack.mkString(""))

    //    println(golden.mkString(""))
    //    println(yours.mkString(""))
    //    println(yoursTraceBack.mkString(""))
  }
}

package FTN

import Chainsaw._
import matlabIO._

import scala.collection.mutable.ArrayBuffer

case class ConvencConfig(constLen: Int, codeGens: Array[Int]) {

  val m = codeGens.length
  val n = 1
  val K = constLen
  val decimalCodeGens = codeGens.map(value => BigInt(value.toString, 8).toInt)
  def encode(input: Int): Int = decimalCodeGens // convenc, used to generate the expected output
    .map(gen => (gen & input) // code gen used as mask
      .toBinaryString.map(_.asDigit).reduce(_ ^ _)) // xorR
    .reverse.zipWithIndex.map { case (i, exp) => i * (1 << exp) }.sum // array of bits -> number
  def branch(state: Int): Array[Int] = Array(state, state + (1 << (K - 1))) // two branch from a current state, add 1/0 to the left

  // infos needed for building the decoder
  val states: Array[Int] = (0 until (1 << (K - 1))).toArray
  val transitions: Array[Array[Int]] = states.map(i => branch(i)) // including all K(rather than K-1 state bits), size = 2^(K-1) * 2
  val nextStates: Array[Array[Int]] = transitions.map(_.map(_ >> 1)) // next states listed in the order of current state, size = 2^(K-1) * 2
  val expectedOutputs: Array[Array[Int]] = transitions.map(_.map(encode)) // expected output which match the transitions

  def checkTrellis(): Unit = { // using the matlab model to for trellis info validation
    val trellis = MatlabRef.poly2trellis(constLen, codeGens)
    val goldenOutputs = trellis.get("outputs").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    val goldenNextStates = trellis.get("nextStates").asInstanceOf[Array[Array[Double]]].map(_.map(_.toInt))
    require(nextStates.formatted == goldenNextStates.formatted, "nextStates different from Matlab")
    require(expectedOutputs.formatted == goldenOutputs.formatted, "expectedOutputs different from Matlab")
  }
  try {
    checkTrellis()
  } catch {
    case _ => println("no matlab environment available, trellis validation skipped ")
  }
}

object Algos {
  def vitdec(bits: Array[Double], config: ConvencConfig, tblen: Int,
             verbose: Boolean = false, debug: Boolean = false): Array[Double] = {

    import config._ // the trellis infos are built in the config

    val k0 = 1
    val n0 = decimalCodeGens.length
    val K = constLen

    // build the trellis infos
    def getHamming(a: Int, b: Int) = (a ^ b).toBinaryString.map(_.asDigit).sum // xor generate 1 when different

    var paths = Seq.fill(1 << K)("")
    var metrics = 0 +: Seq.fill(1 << K - 1)(n0 * tblen) // TODO: a safe init value, could be smaller in magnitude
    var selectionsRAM = ArrayBuffer[String]()
    var determinedBits = ""

    // the decoding process: keep writing selections to the RAM
    val frames = bits.grouped(n0).toArray
    frames.indices.foreach { i =>
      val frameValue: Int = frames(i).map(_.toInt).reverse.zipWithIndex.map { case (d, i) => d * (1 << i) }.sum // array of bits -> number
      val deltaMetrics = expectedOutputs.map(_.map(getHamming(_, frameValue))) // BMU

      val candidateMetrics = metrics.zip(deltaMetrics).map { case (metric, hams) => hams.map(_ + metric) }
      val candidatePaths = paths.map(path => Array("0", "1").map(bit => path + bit)) // record the full path in software for debugging
      val selectionPairs = nextStates.flatten.zip(candidateMetrics.flatten.zip(candidatePaths.flatten)) // next states with candidate metrics and paths
        .map { case (state, metricAndPath) => state -> metricAndPath }.sortBy(_._1).grouped(n0).toSeq // group the candidates for each next state
      val updated = selectionPairs.map(_.minBy(_._2._1)).toArray // keep the one with smallest metric

      val metricPairs = selectionPairs.map(_.map(_._2._1))
      val selections = metricPairs.map(pair => if (pair(0) <= pair(1)) 0 else 1)
      if(debug){
        println(s"at frame $i")
        println(s"hammings lower to higher")
        println(s"selections lower to higher: ${BigInt(selections.mkString(""), 2).toString(16)}")
      }

      def int2Bin = (value: Int) => BigInt(value).toString(2).padToLeft(K - 1, '0')
      def tabulate(): Unit = { // TODO: find a framework to deal with these problems, something like pandas
        println("-" * 6 * (K + 2))
        println(s"frame $i")
        val nextStateStrings = nextStates.map(_.map(value => int2Bin(value)))
        val fullString = states.map { state =>
          val line0 = Seq(int2Bin(state), nextStateStrings(state)(0),
            int2Bin(expectedOutputs(state)(0)),
            metrics(state).toString, deltaMetrics(state)(0).toString,
            candidateMetrics(state)(0).toString.padToLeft(2, ' '),
            "  " + paths(state))
            .map(_.padToLeft(K + 2, ' ')).mkString("")
          val line1 = Seq("", nextStateStrings(state)(1),
            int2Bin(expectedOutputs(state)(1)),
            "", deltaMetrics(state)(1).toString,
            candidateMetrics(state)(1).toString.padToLeft(2, ' '),
            "")
            .map(_.padToLeft(K + 2, ' ')).mkString("")
          Seq(line0, line1).mkString("\n")
        }.mkString("\n")

        println("-" * 6 * (K + 2))
        println("received: " + frames(i).map(_.toInt).mkString(""))
        println(Seq("s", "ns", "o", "m", "ham", "nm", "path").map(_.padToLeft(K + 2, ' ')).mkString(""))
        println("-" * 6 * (K + 2))
        println(fullString)
        println(s"true result bits: ${paths.head}")
        println(s"determined bits: $determinedBits")
        // no effect when paths have agreement on the first bit
        if (i > 0) println(s"fixed tblen has no effect: ${paths.forall(path => path(0) == paths.head(0))}")
      }
      if (verbose) tabulate()

      // 2-forward and 1-backward
      def traceBack(partialSelections: Array[String], end: Boolean = false, verbose: Boolean = false): String = { // trace back, according to the previous selections
        var currentState = 0
        var outputBits = ""
        partialSelections.reverse.foreach { selections =>
          if (verbose) print(s"${int2Bin(currentState)} -> ${selections(currentState)} -> ")
          val previousState = int2Bin(currentState).tail + selections(currentState) // concatenate the selection bit to the right and drop the leftmost bit
          outputBits += int2Bin(currentState).head // export the leftmost bit which is the
          currentState = BigInt(previousState, 2).toInt
        }
        if (end) outputBits.reverse else outputBits.reverse.take(tblen)
      }

      metrics = updated.map(_._2._1)
      paths = updated.map(_._2._2)

      // the control logic of traceback: traceback when RAM is full / at the end
      val end = i == frames.length - 1
      selectionsRAM += selections.mkString("")
      if (selectionsRAM.length == 2 * tblen || end) {
        if (verbose) println(s"traceback at $i")
        determinedBits += traceBack(selectionsRAM.toArray, end)
        if (!end) selectionsRAM = selectionsRAM.takeRight(tblen) else selectionsRAM.clear()
      }
    }

    //    val ret = determinedBits + paths.head.takeRight(tblen) // the way that saves the paths
    val ret = determinedBits // the way that saves the previous state and trace back
    ret.map(_.asDigit.toDouble).toArray
  }

  // generate testCase for vitdec
  def vitdecTestCase(config: ConvencConfig, testCaseLen: Int, noiseNumber: Int = 0) = {
    import config._
    val flushingBits: Seq[Double] = Seq.fill(constLen - 1)(0.0) // bits to reset the decoder registers, assuring the end state is S_0(all zero)
    val testCase: Array[Double] = ((0 until testCaseLen).map(_ => DSPRand.nextInt(2).toDouble) ++ flushingBits).toArray
    val trellis: MStruct = MatlabRef.poly2trellis(constLen, codeGens)
    val coded: Array[Double] = MatlabRef.convenc(testCase, trellis)
    // random error bits
    (0 until noiseNumber).foreach(_ => coded(DSPRand.nextInt(testCaseLen)) = 1 - coded(DSPRand.nextInt(testCaseLen)))
    coded
  }

  def main(args: Array[String]): Unit = {

    val constLen = 7
    val codeGens = Array(171, 133)
    val tblen = constLen * 6
    val testCaseLen = 30 * constLen
    val noiseNumber = 5 // number of random errors

    //         to explore the viterbi algo, use the following example of (2,1,3) convolutional code
    //        val constLen = 3
    //        val codeGens = Array(7, 4)
    //        val tblen = constLen * 6
    //        val testCaseLen = 100

    var success = 0
    val total = 1000
    (0 until total).foreach { i =>
      val coded = vitdecTestCase(ConvencConfig(constLen, codeGens), testCaseLen, noiseNumber)
      val trellis = MatlabRef.poly2trellis(constLen, codeGens)
      val golden = MatlabRef.vitdec(coded, trellis, tblen)
      val ours = vitdec(coded, ConvencConfig(constLen, codeGens), tblen, verbose = false)
      if (golden.map(_.toInt).mkString("") == ours.map(_.toInt).mkString("")) success += 1
      //      require(golden.map(_.toInt).mkString("") == ours.map(_.toInt).mkString(""),
      //        s"golden: ${golden.map(_.toInt).mkString("")}\n" +
      //          s"yours:  ${ours.map(_.toInt).mkString("")}"
      //      )
      if (i % 100 == 99) println(s"${(i / 100 + 1) * 100} done, $success / ${(i / 100 + 1) * 100} succeed")
    }
    printlnGreen(s"vitdec, $success / $total succeed ")
  }
}

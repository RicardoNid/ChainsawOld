package FTN

import matlabIO._
import Chainsaw._

object Algos {
  def vitdec(bits: Array[Double], constLen: Int, codeGens: Array[Int], tblen: Int, verbose: Boolean = false): Array[Double] = {

    val decimalCodeGens = codeGens.map(value => BigInt(value.toString, 8).toInt)
    val k0 = 1
    val n0 = decimalCodeGens.size
    val K = constLen

    // matlab add to left and shift right to construct next states
    def branch(state: Int): Array[Int] = Array(state, state + (1 << (K - 1))) // two branch from a current state, add 1/0 to the left
    def encode(input: Int): Int = decimalCodeGens // convenc
      .map(gen => BigInt(gen & input) // code gen as mask
        .toString(2).map(_.asDigit).reduce(_ ^ _)) // xorR
      .reverse.zipWithIndex.map { case (i, exp) => i * (1 << exp) }.sum
    def getHamming(a: Double, b: Double) = (BigInt(a.toInt) ^ BigInt(b.toInt)) // xor: finding differences
      .toString(2).map(_.asDigit).sum

    // build the trellis
    val outputs = (0 until (1 << (K - 1)))
      .map(i => branch(i).map(encode).map(_.toDouble)).toArray
    val nextStates = (0 until (1 << (K - 1)))
      .map(i => branch(i).map(_ >> 1).map(_.toDouble)).toArray

    def checkTrellis() = {
      val trellis = MatlabRef.poly2trellis(constLen, codeGens)
      val goldenOutputs = trellis.get("outputs").asInstanceOf[Array[Array[Double]]]
      val goldenNextStates = trellis.get("nextStates").asInstanceOf[Array[Array[Double]]]
      require(nextStates.formatted == goldenNextStates.formatted, "nextStates different from Matlab")
      require(outputs.formatted == goldenOutputs.formatted, "nextStates different from Matlab")
    }
    checkTrellis()

    var paths = Seq.fill(1 << K)(" " * tblen)
    var metrics = 0 +: Seq.fill(1 << K - 1)(n0 * tblen) // TODO: a safe init value, could be smaller in magnitude
    var determinedBits = ""
    val frames = bits.grouped(n0).toArray
    // the decoding process
    frames.indices.foreach { i =>
      val frameValue = frames(i).reverse.zipWithIndex.map { case (d, i) => d * (1 << i) }.sum
      val hammings = outputs.map(_.map(getHamming(_, frameValue))) // BMU
      val candidateMetrics = metrics.zip(hammings).map { case (metric, hams) => hams.map(_ + metric) }
      val candidatePaths = paths.map(path => Array("0", "1").map(bit => (path + bit).takeRight(tblen)))
      val updated = nextStates.flatten.zip(candidateMetrics.flatten.zip(candidatePaths.flatten)) // next states with candidate metrics and paths
        .map { case (state, metricAndPath) => state -> metricAndPath }.sorted.grouped(n0) // group the candidates for each next state
        .map(_.sortBy(_._2._1).head).toArray // keep the one with smallest metric

      def tabulate() = { // TODO: learn a framework to deal with these problems, something like pandas
        val indices = 0 until 1 << (K - 1)
        def int2Bin = (value: Int) => BigInt(value).toString(2).padToLeft(K - 1, '0')
        val stateStrings = indices.map(int2Bin)
        val nextStateStrings = nextStates.map(_.map(value => int2Bin(value.toInt)))

        val fullString = indices.map { j =>
          val line0 = Seq(stateStrings(j), nextStateStrings(j)(0),
            int2Bin(outputs(j)(0).toInt),
            metrics(j).toString, hammings(j)(0).toString,
            candidateMetrics(j)(0).toString.padToLeft(2, ' '),
            "  " + paths(j))
            .map(_.padToLeft(K + 2, ' ')).mkString("")
          val line1 = Seq("", nextStateStrings(j)(1),
            int2Bin(outputs(j)(1).toInt),
            "", hammings(j)(1).toString,
            candidateMetrics(j)(1).toString.padToLeft(2, ' '),
            "")
            .map(_.padToLeft(K + 2, ' ')).mkString("")
          Seq(line0, line1).mkString("\n")
        }.mkString("\n")

        println("-" * 6 * (K + 2))
        println("received: " + frames(i).map(_.toInt).mkString(""))
        println(Seq("s", "ns", "o", "m", "ham", "nm", "path").map(_.padToLeft(K + 2, ' ')).mkString(""))
        println("-" * 6 * (K + 2))
        println(fullString)
        println(s"true result bits: ${paths(0)}")
        println(s"determined bits: $determinedBits")
        // no effect when paths have agreement on the first bit
        println(s"fixed tblen has no effect: ${paths.forall(path => path(0) == paths(0)(0))}")
      }
      if (verbose) tabulate()

      metrics = updated.map(_._2._1)
      if (i >= tblen) determinedBits += paths(0)(0)
      paths = updated.map(_._2._2)
    }

    val ret = determinedBits + paths(0).takeRight(tblen)
    ret.map(_.asDigit.toDouble).toArray
  }

  def main(args: Array[String]): Unit = {

    //    val constLen = 7
    //    val codeGens = Array(171, 133)
    //    val tblen = constLen * 6
    //    val testCaseLen = 2000 * constLen

    //     to explore the viterbi algo, use the following example of (2,1,3) convolutional code
    val constLen = 3
    val codeGens = Array(7, 4)
    val tblen = constLen * 3
    val testCaseLen = 20

    (0 until 1).foreach { _ =>
      val flushingBits = Seq.fill(constLen - 1)(0.0) // bits to reset the decoder registers, assuring the end state is S_0(all zeor)
      val testCase = ((0 until testCaseLen).map(_ => DSPRand.nextInt(2).toDouble) ++ Seq.fill(constLen - 1)(0.0)).toArray
      val trellis = MatlabRef.poly2trellis(constLen, codeGens)
      val coded = MatlabRef.convenc(testCase, trellis)
      // a random noise, when this is applied, tblen = K is not enough, in our test, we take tblen = 5K
      coded(DSPRand.nextInt(testCaseLen)) = 1 - coded(DSPRand.nextInt(testCaseLen))
      val golden = MatlabRef.vitdec(coded, trellis, tblen)

      val ours = vitdec(coded, constLen, codeGens, tblen, verbose = true)
      val inputString = testCase.map(_.toInt).mkString("")
      val ourString = ours.map(_.toInt).mkString("") // TODO: find out the condition for matlab to have correct result
      println(inputString)
      require(inputString == ourString)
    }
    printlnGreen("vitdec succeed")

  }
}

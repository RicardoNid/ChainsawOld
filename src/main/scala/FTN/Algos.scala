package FTN

import matlabIO._
import Chainsaw._

object Algos {
  def vitdec(bits: Array[Double], constLen: Int, codeGens: Array[Int], tblen: Int): Array[Double] = {
    // TODO: replace with my own implementations
    val trellis = MatlabRef.poly2trellis(constLen, codeGens)

    val decimalCodeGens = codeGens.map(value => BigInt(value.toString, 8).toInt)
    val k0 = 1
    val n0 = decimalCodeGens.size
    val K = constLen

    var paths = Seq.fill(1 << K)("")
    var metrics = 0 +: Seq.fill(1 << K - 1)(n0 * tblen) // a safe init value, could be smaller in magnitude

    // matlab add to left and shift right to construct next states
    def branch(state: Int) = Array(state, state + (1 << (K - 1))) // two branch from a current state, add 1/0 to the left
    def encode(input: Int) = decimalCodeGens
      .map(gen => BigInt(gen & input) // code gen as mask
        .toString(2).map(_.asDigit).reduce(_ ^ _)) // xorR
      .reverse.zipWithIndex.map { case (i, exp) => i * (1 << exp) }.sum
    def getHamming(a: Double, b: Double) = (BigInt(a.toInt) ^ BigInt(b.toInt)).toString(2).map(_.asDigit).sum

    val outputs = (0 until (1 << (K - 1)))
      .map(i => branch(i).map(encode).map(_.toDouble)).toArray
    val nextStates = (0 until (1 << (K - 1)))
      .map(i => branch(i).map(_ >> 1).map(_.toDouble)).toArray

    val goldenOutputs = trellis.get("outputs").asInstanceOf[Array[Array[Double]]]
    val goldenNextStates = trellis.get("nextStates").asInstanceOf[Array[Array[Double]]]

    println(outputs.formatted)
    println("-" * 20)
    println(goldenOutputs.formatted)

    require(nextStates.formatted == goldenNextStates.formatted)
    require(outputs.formatted == goldenOutputs.formatted)


    val frames = bits.grouped(n0).toArray
    frames.indices.foreach { i =>
      val frameValue = frames(i).reverse.zipWithIndex.map { case (d, i) => d * (1 << i) }.sum
      val hammings = outputs.map(_.map(getHamming(_, frameValue)))
      val candidateMetrics = metrics.zip(hammings).map { case (metric, hams) => hams.map(_ + metric) }
      val candidatePaths = paths.map(path => Array("0", "1").map(path + _))
      val updated = nextStates.flatten.zip(candidateMetrics.flatten.zip(candidatePaths.flatten)) // next states with candidate metrics and paths
        .map { case (state, metricAndPath) => state -> metricAndPath }.sorted.grouped(n0) // group the candidates for each next state
        .map(_.sortBy(_._2._1).head).toArray // keep the one with smallest metric

      def tabulate() = {
        val indices = 0 until 1 << (K - 1)
        def int2Bin = (value: Int) => BigInt(value).toString(2).padToLeft(K - 1, '0')
        val stateStrings = indices.map(int2Bin)
        val nextStateStrings = nextStates.map(_.map(value => int2Bin(value.toInt)))

        val fullString = indices.map { j =>
          val line0 = Seq(stateStrings(j), nextStateStrings(j)(0),
            int2Bin(outputs(j)(0).toInt),
            metrics(j).toString, hammings(j)(0).toString,
            candidateMetrics(j)(0).toString.padToLeft(2, ' '))
            .map(_.padToLeft(K + 2, ' ')).mkString("")
          val line1 = Seq("", nextStateStrings(j)(1),
            int2Bin(outputs(j)(1).toInt),
            "", hammings(j)(1).toString,
            candidateMetrics(j)(1).toString.padToLeft(2, ' '))
            .map(_.padToLeft(K + 2, ' ')).mkString("")
          Seq(line0, line1).mkString("\n")
        }.mkString("\n")

        println("received: " + frames(i).map(_.toInt).mkString(""))
        println(Seq("state","next","output","metric","hamming","new").map(_.padToLeft(K + 2, ' ')).mkString(""))
        println(fullString)
      }
      tabulate()

      metrics = updated.map(_._2._1)
      paths = updated.map(_._2._2)

      println("-" * 20)
    }

    paths(0).map(_.asDigit.toDouble).toArray
  }

  def main(args: Array[String]): Unit = {
    val constLen = 7
    val codeGens = Array(171, 133)
    //    val constLen = 3
    //    val codeGens = Array(7, 4)
    val tblen = constLen
    val testCaseLen = 20 * constLen

    val testCase = ((0 until testCaseLen).map(_ => DSPRand.nextInt(2).toDouble) ++ Seq.fill(tblen)(0.0)).toArray

    val trellis = MatlabRef.poly2trellis(constLen, codeGens)
    val coded = MatlabRef.convenc(testCase, trellis)
    val golden = MatlabRef.vitdec(coded, trellis, tblen)

    val ours = vitdec(coded, constLen, codeGens, 10)
    val inputString = testCase.map(_.toInt).mkString("")
    val ourString = ours.map(_.toInt).mkString("") // TODO: find out the condition for matlab to have correct result
    if(inputString == ourString) printlnGreen("vitdec succeed")



    println(golden.map(_.toInt).reverse.mkString(""))
  }
}

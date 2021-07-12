package FTN

import matlabIO._
import Chainsaw._

object Algos {
  def vitdec(bits: Array[Double], constLen: Int, codeGens: Array[Int], tblen: Int): Array[Array[Double]] = {
    val k0 = 1
    val n0 = codeGens.size
    val K = constLen

    var paths = Seq.fill(1 << K)("")
    var metrics = 0 +: Seq.fill(1 << K - 1)(n0 * tblen) // a safe init value, could be smaller in magnitude

    // TODO: replace with my own implementations
    val trellis = MatlabRef.poly2trellis(constLen, codeGens)

    // matlab add to left and shift right to construct next states
    def branch(state: Int) = Array(state, state + (1 << (K - 1)))
    def encode(input: Int) = codeGens
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
    require(outputs.formatted == goldenOutputs.formatted)
    require(nextStates.formatted == goldenNextStates.formatted)

    val frames = bits.grouped(n0).toArray
    frames.indices.foreach { i =>
      val frameValue = frames(i).reverse.zipWithIndex.map{ case (d, i) => d * (1 << i)}.sum
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
          val line0 = Seq(stateStrings(j).padTo(K, ' '), nextStateStrings(j)(0),
            int2Bin(outputs(j)(0).toInt),
            metrics(j).toString.padToLeft(2, ' '), hammings(j)(0),
            candidateMetrics(j)(0).toString.padToLeft(2, ' ')).mkString("  ")
          val line1 = Seq(" " * K, nextStateStrings(j)(1),
            int2Bin(outputs(j)(1).toInt),
            " " * 2, hammings(j)(1),
            candidateMetrics(j)(1).toString.padToLeft(2, ' ')).mkString("  ")
          Seq(line0, line1).mkString("\n")
        }.mkString("\n")

        println("received: " + frames(i).map(_.toInt).mkString(""))
        println("state next output metric hamming new")
        println(fullString)
      }
      tabulate()

      metrics = updated.map(_._2._1)
      paths = updated.map(_._2._2)

      println("-" * 20)
    }

    println(s"result ${paths(0)}")

    nextStates
  }

  def main(args: Array[String]): Unit = {
    val trellis = MatlabRef.poly2trellis(3, Array(7, 4))
    val coded = MatlabRef.convenc(Array(1.0, 0, 1, 0, 0, 0), trellis)
    val golden = MatlabRef.vitdec(coded, trellis, tblen = 3)
    vitdec(coded, 3, Array(7, 4), 10).formatted
    println(coded.mkString(" "))
    println(golden.mkString(" "))
  }
}

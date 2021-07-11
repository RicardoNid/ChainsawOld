package FTN

import matlabIO._

object Algos {
  def vitdec(bits: Array[Double], constLen: Int, codeGen: Array[Int], tblen: Int): Array[Array[Double]] = {
    val k0 = 1
    val n0 = codeGen.size
    val K = constLen

    val paths = Seq.fill(1 << K)("")
    val metrics = 0 +: Seq.fill(1 << K - 1)(-n0 * tblen) // a safe init value, could be smaller in magnitude

    // TODO: replace with my own implementations
    val trellis = MatlabRef.poly2trellis(constLen, codeGen)
    val outputs = trellis.get("outputs").asInstanceOf[Array[Array[Double]]]
    val nextStates = trellis.get("nextStates").asInstanceOf[Array[Array[Double]]]

    def getHamming(a: Double, b: Double) = (BigInt(a.toInt) ^ BigInt(b.toInt)).toString(2).map(_.asDigit).sum

    val frames = bits.grouped(n0).toArray
    frames.indices.foreach { i =>
      val hammings = outputs.foreach(output => output.zip(frames(i)).map { case (o, r) => getHamming(o, r) })

    }

    nextStates
  }

  def main(args: Array[String]): Unit = {
    println(vitdec(Array(1.0), 7, Array(171, 133), 10).formatted)
  }
}

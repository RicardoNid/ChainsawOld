package matlabIO

object FTNDemo {
  def main(args: Array[String]): Unit = {
    val eng = AsyncEng.get()

    def poly2trellis(constLen: Int, codeGen: Array[Int]): MStruct = eng.feval[MStruct]("poly2trellis", Array(constLen.toDouble), codeGen.map(_.toDouble))
    def vitdec(bits: Array[Double], trellis: MStruct, tblen: Int): Array[Double] =
      eng.feval[Array[Double]]("vitdec", bits, trellis, Array(tblen.toDouble), "cont", "hard")



    // conv parameters
    val ConvCodeRate = 0.5
    val ConvConstLen = 7
    val ConvCodeGen = Array(171, 133)
    val ConvTrellis = poly2trellis(ConvConstLen, ConvCodeGen)

    val tblen = 90
    println(ConvTrellis.formatted)


    import Chainsaw.DSPRand
    println(vitdec(
      bits = (0 until 10).map(_ => DSPRand.nextInt(2).toDouble).toArray,
      ConvTrellis,
      4
    ).mkString(" "))
  }
}

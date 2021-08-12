package Chainsaw.DSP.FFT

import matlabIO._

object Algos {

  // build radix-r Cooley-Tukey FFT by the "block" and "parallel line" given
  def cooleyTukeyBuilder[T](input: Seq[T], factors: Seq[Int],
                            block: Seq[T] => Seq[T],
                            mult: (T, Int, Int) => T): Seq[T] = {

    def twiddle(input: Seq[Seq[T]]) = {
      val N1 = input.head.size
      val N2 = input.size
      input.zip(cooleyTukeyCoeffIndices(N1, N2))
        .map { case (ts, ints) => ts.zip(ints)
          .map { case (t, i) => mult(t, i, N1 * N2) }
        }
    }

    def recursiveBuild(input: Seq[T], factors: Seq[Int]): Seq[T] = {

      if (factors.size == 1) block(input)
      else {
        val N1 = factors.head
        val N2 = input.size / N1

        val input2D = Seq.tabulate(N2, N1)((n2, n1) => input(N2 * n1 + n2)) // 2D, each segment in a row
        val afterBlock = input2D.map(block)
        val afterParallel = twiddle(afterBlock)
        val input2DForRecursion = Seq.tabulate(N1, N2)((k1, n2) => afterParallel(n2)(k1))
        val afterRecursion = input2DForRecursion.map(recursiveBuild(_, factors.tail))
        val ret = Seq.tabulate(N2, N1)((k2, k1) => afterRecursion(k1)(k2)).flatten
        ret
      }
    }
    recursiveBuild(input, factors)
  }

  def cooleyTukeyCoeffIndices(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)
  def cooleyTukeyCoeff(N1: Int, N2: Int) = cooleyTukeyCoeffIndices(N1, N2).map(_.map(WNnk(N1 * N2, _)))

  def cooleyTukeyFFT(input: Seq[MComplex], factors: Seq[Int]) = {
    def block(input: Seq[MComplex]) = eng.feval[Array[MComplex]]("fft", input.toArray).toSeq
    def mult(input: MComplex, index: Int, N: Int) = input * WNnk(N, index)
    cooleyTukeyBuilder(input, factors, block, mult)
  }

  def radixRFFT(input: Seq[MComplex], radix: Int) = {
    val N = input.size
    def buildFactors(factors: Seq[Int]): Seq[Int] = if (factors.product == N) factors else buildFactors(factors :+ radix)
    cooleyTukeyFFT(input, buildFactors(Seq(radix)))
  }

}

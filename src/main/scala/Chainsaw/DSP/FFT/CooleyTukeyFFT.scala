package Chainsaw.DSP.FFT

import spinal.core._
import matlabIO._

// TODO: implement

/** N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm, DSP with FPGA, algo 6.8, fig 6.12
 *
 * @param N length of FFT
 */
class CooleyTukeyFFT(N: Int) extends ImplicitArea[Vec[SFix]] {

  def primitiveAlgo(input: Array[MComplex], N2: Int) = {
    require(N % N2 == 0)
    val N1 = N / N2

    // reorder and group
    def interleave(input: Seq[MComplex], group: Int): Seq[Array[MComplex]] = {
      val groupSize = input.size / group
      val indices = Array.tabulate(group, groupSize)(_ + _ * group).flatten
      indices.map(input(_)).grouped(groupSize).toSeq
    }

    // property of interleave
    //    val test = (1 until 12).map(i => new MComplex(i, i)).toArray
    //    println(interleave(interleave(test,3).flatten,3).flatten.mkString(" "))

    // group the input elements by n2, results in N2 groups for N1-point DFT
    val N2Groups: Seq[Array[MComplex]] = interleave(input, N2)
    val stage1Results: Seq[MComplex] = N2Groups.map(Refs.FFT(_)).flatten // sub-DFT, can be substituted by any valid algo

    val twiddleFactors = Array.tabulate(N2, N1)((n2: Int, k1: Int) => WNnk(N, n2 * k1)).flatten
    val twiddeledResults = stage1Results.zip(twiddleFactors).map(pair => pair._1 * pair._2).toArray

    // group the results by k1
    val N1Groups: Seq[Array[MComplex]] = interleave(twiddeledResults, N1)
    val stage2Results = N1Groups.map(Refs.FFT(_)).flatten // sub-DFT

    // validate
    val orderedResult = interleave(stage2Results, N1).flatten
    assert(orderedResult.zip(Refs.FFT(input)).forall(pair => pair._1.sameAs(pair._2)))
    orderedResult
  }

  override def implicitValue = ???

}

object CooleyTukeyFFT extends App {
  def apply(N: Int): CooleyTukeyFFT = new CooleyTukeyFFT(N)
  CooleyTukeyFFT(12).primitiveAlgo(Array.fill(12)(new MComplex(1, 1)), 3)
}
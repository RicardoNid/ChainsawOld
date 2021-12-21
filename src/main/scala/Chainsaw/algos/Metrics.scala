package Chainsaw.algos

object Metrics {

  def Hamming(expected: Int, observed: Int): Double = (expected ^ observed).toBinaryString.map(_.asDigit).sum

}

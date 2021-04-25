package DSP

import DSP.ModularMultiplication.getModularInverse
import spinal.core._
import spinal.lib._


/** Kernel for modular multiplication by Montgomery
 *
 * @param N modulo
 */
class ModularMultiplication(A: UInt, B: UInt, N: Int) extends ImplicitArea[UInt] with DSPDesign with Testable {

  val start = Bool()
  val busy = Bool()

  val innerWidth = log2Up(N - 1)
  // precompute statically
  val R = 1 << log2Up(N)
  val RInverse = getModularInverse(R, N)
  val RSquare = (R * R) % N
  val NPrime = (R * RInverse - 1) / N

  def REDC(T: UInt) = {
    val m = RegNext((T(innerWidth - 1 downto 0) * U(NPrime, innerWidth bits)) (innerWidth - 1 downto 0))
    val t = (T + m * U(NPrime, innerWidth bits)) >> innerWidth
    t
  }

  val AR = RegNext(REDC(A(innerWidth - 1 downto 0) * U(RSquare, innerWidth bits)))
  val BR = RegNext(REDC(B(innerWidth - 1 downto 0) * U(RSquare, innerWidth bits)))
  val product = (AR * BR) (innerWidth - 1 downto 0)
  val ABR = RegNext(REDC(product))

  override def implicitValue = RegNext(REDC(ABR))

  override val getTimingInfo: TimingInfo = TimingInfo(inputInterval = 1, outputInterval = 1, latency = LatencyAnalysis(A, implicitValue), initiationInterval = 1)
}

object ModularMultiplication {

  def getModularInverse(n: Int, modulo: Int) = {
    (1 until modulo).filter(_ * n % modulo == 1).head
  }

  def main(args: Array[String]): Unit = {
    println(getModularInverse(16, 13))
  }
}

package projects

import breeze.numerics.floor

package object FTN {
  val projectSrcs = "C:/Users/lsfan/Documents/GitHub/FTN/FTN.srcs"
  val outputDir = "./output/verilog/examples"

  import spinal.core._
  import spinal.lib._
  import spinal.lib.fsm._

  val testFFTLength = 8

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  def Double2Fix(value: Double) = floor(value * (1 << fractionalWidth)).toInt // convert Double to valid stimulus for simulation

  def isPrime(n: Int): Boolean =
    if (n <= 1)
      false
    else if (n == 2)
      true
    else
      !(2 until n).exists(n % _ == 0)

  def complexMultiplier(C: Double, S: Double, X: SFix, Y: SFix) = {
    val Cfix, Sfix = data
    Cfix := C
    Sfix := S

    val E = X - Y
    val Z = Cfix * E

    val R = ((Cfix - Sfix) * Y + Z).truncated
    val I = ((Cfix + Sfix) * X - Z).truncated

    (R, I)
  }

  def winogradDFT(N: Int, input: IndexedSeq[ComplexNumber]) = {
    require(isPrime(N), s"Winograd DFT is for prime number")
    require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

    val output = Array.ofDim[ComplexNumber](N)
    if (N == 2) {
      val a0 = input(0)
      val a1 = input(1)
      val s0 = a0 + a1
      val s1 = a0 - a1
      val m0 = s0
      val m1 = s1
      output(0) = m0
      output(1) = m1
    }
    output
  }


  val CDConfig = ClockDomainConfig(
    resetActiveLevel = LOW,
    resetKind = ASYNC
  )

  // typedefs
  val naturalWidth = 8
  val fractionalWidth = 8
  val bitWidth = naturalWidth + fractionalWidth

  // axi stream = stream + fragment + user
  case class AXIS(dataWidth: Int,
                  hasLast: Boolean,
                  userWidth: Int) extends Bundle with IMasterSlave {

    val stream = Stream((Bits(dataWidth bits)))
    val last = if (hasLast) Bool else null
    val user = Bits(userWidth bits)

    override def asMaster(): Unit = {
      master(stream)
      out(user)
      if (hasLast) out(last)
    }
  }

  val default_vector = Array(6, 0, -4, -3, 5, 6, -6, -13, 7, 44, 64, 44, 7, -13, -6, 6, 5, -3, -4, 0, 6)
}

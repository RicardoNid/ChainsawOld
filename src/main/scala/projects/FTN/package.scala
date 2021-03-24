package projects

import breeze.numerics.floor

package object FTN {
  val projectSrcs = "C:/Users/lsfan/Documents/GitHub/FTN/FTN.srcs"
  val outputDir = "./output/verilog/examples"

  import spinal.core._
  import spinal.lib._
  import spinal.lib.fsm._

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)
  def Double2Fix(value: Double) = floor(value * (1 << fractionalWidth)).toInt // convert Double to valid stimulus for simulation

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


  val CDConfig = ClockDomainConfig(
    resetActiveLevel = LOW,
    resetKind = ASYNC
  )

  // typedefs
  val naturalWidth = 8
  val fractionalWidth = 8
  val bitWidth = naturalWidth + fractionalWidth

  case class ComplexNumber(r: Double,
                           i: Double) {

    def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

    val real, imag = data
    real := r
    imag := i
  }

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

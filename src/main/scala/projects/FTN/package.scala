package projects

package object FTN {
  val projectSrcs = "C:/Users/lsfan/Documents/GitHub/FTN/FTN.srcs"
  val outputDir = "./output/verilog/examples"

  import spinal.core._
  import spinal.lib._
  import spinal.lib.fsm._

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
    val last = if(hasLast) Bool else null
    val user = Bits(userWidth bits)

    override def asMaster(): Unit = {
      master(stream)
      out(user)
      if(hasLast) out(last)
    }
  }

  val default_vector = Array(6, 0, -4, -3, 5, 6, -6, -13, 7, 44, 64, 44, 7, -13, -6, 6, 5, -3, -4, 0, 6)
}

package object FTN {
  val projectSrcs = "C:/Users/lsfan/Documents/GitHub/FTN/FTN.srcs"
  val outputDir = "./output/verilog/examples"

  import spinal.core._
  import spinal.lib._
  import spinal.lib.fsm._

  val XilinxCDConfig = ClockDomainConfig( // Xilinx UG901推荐的设计范式
    clockEdge = RISING,
    resetKind = BOOT, // BOOT类型太重要了
    resetActiveLevel = LOW,
    softResetActiveLevel = LOW,
    clockEnableActiveLevel = HIGH
  )

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

    // todo : 定义方法令两个AXIS端口的user字段可以进行直连/指定delay的连接
  }

  val default_vector = Array(6, 0, -4, -3, 5, 6, -6, -13, 7, 44, 64, 44, 7, -13, -6, 6, 5, -3, -4, 0, 6)
}

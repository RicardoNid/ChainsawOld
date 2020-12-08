import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

package object OFDM {
  val projectSrcs = "C:\\Users\\lsfan\\Documents\\GitHub\\OFDM\\Spinal"

  val OFDMCDConfig = ClockDomainConfig( // 本工程采用的时钟域策略
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = LOW,
    softResetActiveLevel = LOW,
    clockEnableActiveLevel = HIGH
  )

  def dataType = SInt(8 bits)

  def shortTrainingSequence = Vec(
    // real
    S"00001100",
    S"11011110",
    S"11111101",
    S"00100100",
    S"00011000",
    S"00100100",
    S"11111101",
    S"11011110",
    S"00001100",
    S"00000001",
    S"11101100",
    S"11111101",
    S"00000000",
    S"11111101",
    S"11101100",
    S"00000001",
    // im
    S"00001100",
    S"00000001",
    S"11101100",
    S"11111101",
    S"00000000",
    S"11111101",
    S"11101100",
    S"00000001",
    S"00001100",
    S"11011110",
    S"11111101",
    S"00100100",
    S"00011000",
    S"00100100",
    S"11111101",
    S"11011110")
}

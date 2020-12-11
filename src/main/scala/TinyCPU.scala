package TinyCPU

import TinyCPU.testBench
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.util.Random

class TinyCPU extends Component {

  def typeWord = UInt(16 bits)

  val ZERO = U"16'h0000"

  val io = new Bundle {
    val clk = in Bool
    val rstn = in Bool
    val pc = out(typeWord)
    val ir = out(typeWord)
    val ac = out(typeWord)
  }

  noIoPrefix()

  val CDConfig = ClockDomainConfig(
    resetKind = SYNC,
    resetActiveLevel = LOW
  )

  val mainCD = ClockDomain(
    clock = io.clk,
    reset = io.rstn,
    config = CDConfig
  )

  val mainArea = new ClockingArea(mainCD) {
    val acReg = RegInit(ZERO)

    val irReg = RegInit(ZERO)
    val opcode = irReg(15 downto 8)
    val address = irReg(7 downto 0)

    val memory = Mem(Bits(16 bits), testBench)

    val pcReg = RegInit(ZERO)
    irReg := memory(pcReg.resized).asUInt
    pcReg := pcReg + 1

    switch(opcode) {
      is(U"8'h00")(acReg := acReg + memory(address.resized).asUInt)
      is(U"8'h01")(memory(address.resized) := acReg.asBits)
      is(U"8'h02")(acReg := memory(address.resized).asUInt)
      is(U"8'h03")(pcReg := address.resized)
      is(U"8'h04")(when(acReg < ZERO)(pcReg := address.resized))
    }

    io.pc := pcReg
    io.ir := irReg
    io.ac := acReg
  }

}

object TinyCPU {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = "C:\\Users\\lsfan\\Documents\\GitHub\\TinyCPU").generate(new TinyCPU)
  }

  def testBench = Array(
    // 数据地址从32开始
    // 初始数据 : M[32] = 1 M[33] = 2
    B"16'h0020", // ADD M[32]
    B"16'h0122", // STORE M[34]
    B"16'h0021", // ADD M[33]
    B"16'h0220", // LOAD M[32]
    B"16'h0310", // JUMP 16
    B"16'h0300", // JUMP 0
    B"16'h0410", // JNEG 16
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    B"16'h0000",
    // data memory
    B"16'h0001",
    B"16'h0002",
    B"16'h0002",
    B"16'h0002"
  )
}

object testTinyCPU {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new TinyCPU).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.mainCD.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(100 * period)
          simSuccess()
        }
      }
  }
}

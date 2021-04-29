package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import xilinx.VivadoFlow

/**
 * @param dataWidth
 * @param depth
 * @param width
 */
class Interleave(dataWidth: Int, depth: Int, width: Int) extends Component {

  //  val input = slave(BRAM(BRAMConfig(dataWidth, factorA * factorB)))
  val input = slave Stream Bits(dataWidth bits)
  val output = master Stream Bits(dataWidth bits)

  val writeRowCounter = Counter(depth, input.fire)
  val writeColumnCouneter = Counter(width, writeRowCounter.willOverflow)
  val readCounter = Counter(depth * width, output.fire)

  val writeAtPing = RegInit(True)
  val readAtPing = RegInit(False)

  val writeOver = writeColumnCouneter.willOverflow
  val readOver = readCounter.willOverflow

  val fsm = new StateMachine {
    val EMPTY = StateEntryPoint()
    val HALF = State()
    val FULL = State()

    EMPTY.whenIsActive {
      when(writeOver) {
        writeAtPing := ~writeAtPing
        goto(HALF)
      }
    }
    HALF.whenIsActive {
      when(writeOver && readOver) {
        writeAtPing := ~writeAtPing
        readAtPing := ~readAtPing
      }.elsewhen(writeOver) {
        writeAtPing := ~writeAtPing
        goto(FULL)
      }.elsewhen(readOver) {
        readAtPing := readAtPing
        goto(EMPTY)
      }
    }
    FULL.whenIsActive {
      when(readOver) {
        readAtPing := ~readAtPing
        goto(HALF)
      }
    }

    input.ready := isActive(HALF) || isActive(EMPTY)
    output.valid := isActive(HALF) || isActive(FULL)

  }

  val ping = Mem(Bits(dataWidth bits), depth * width)
  val pong = Mem(Bits(dataWidth bits), depth * width)

  val writeAddr = UInt(ping.addressWidth bits)
  writeAddr := (writeRowCounter * U(width) + writeColumnCouneter).resized
  val readAddr = readCounter

  when(input.fire && writeAtPing)(ping(writeAddr) := input.payload)
  when(input.fire && !writeAtPing)(pong(writeAddr) := input.payload)
  when(output.fire && readAtPing)(output.payload := ping(readAddr))
    .elsewhen(output.fire && !readAtPing)(output.payload := pong(readAddr))
    .otherwise(output.payload := B(0))
}

object Interleave {
  def main(args: Array[String]): Unit = {

    SpinalConfig().generateSystemVerilog(new Interleave(2, 32, 32)) // gen
    SimConfig.withWave.compile(new Interleave(2, 32, 32)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.input.valid #= false
      dut.output.ready #= false
      dut.clockDomain.waitSampling()

      def write = {
        (0 until 20).foreach { i =>
          dut.input.valid #= true
          dut.input.payload #= i
          dut.clockDomain.waitSampling()
        }
      }

      def read = {
        (0 until 20).foreach { i =>
          dut.output.ready #= true
          dut.clockDomain.waitSampling()
        }
      }
    } // sim

    val report = VivadoFlow(new Interleave(2, 32, 32), topModuleName = "FTNInterleave", workspacePath = "output/FTN/Interleave", force = true).doit()
    report.printArea
    report.printFMax

  }
}
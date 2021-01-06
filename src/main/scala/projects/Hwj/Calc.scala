package projects.Hwj

import spinal.core.Component.push
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import sysu.xilinx._
import sysu.util._
import sysu.CNN._

// 前端中断 : 不支持中断,
// 后端中断 :
class Calc(dataWidth: Int, weightWidth: Int, conv: ToeplitzConv) extends Component {

  import conv._

  val io = new Bundle {
    val CalcWeightIn = slave Stream (Vec(Vec(SInt(weightWidth bits), Ww), Wh))
    val CalcDataIn = slave Stream (Vec(Vec(SInt(dataWidth bits), Iw), Ih))
    val CalcDataOut = master Stream (Fragment(Vec(Vec(SInt(dataWidth bits), Iw), Wh)))
  }

  // datapath
  // GEMM
  val calc = new GEMM(weightWidth, dataWidth, Wh, Ww, Iw)
  val partialOutput = Vec(Vec(SInt(calc.resultWidth bits), Iw), Wh)
  calc.io.matrixA := io.CalcWeightIn.payload
  calc.io.matrixB := io.CalcDataIn.payload
  partialOutput := calc.io.matrixO

  // partial sum summing
  val countInner = (roundUp(Nox, Iw) / Iw).toInt
  val countOuter = (roundUp(Nof, Wh) / Wh).toInt

  val counterInner = Counter(countInner, True)
  val counterOuter = Counter(countOuter, counterInner.willOverflow)

  val chainInput = Vec(Vec(SInt(calc.resultWidth bits), Iw), Wh)
  val delayChain = Delay(chainInput, countInner)

  when(counterOuter.value === 0)(chainInput := partialOutput)
    .otherwise {
      for (i <- 0 until Wh; j <- 0 until Iw) chainInput(i)(j) := partialOutput(i)(j) + delayChain(i)(j)
    }

  val fireIn = Delay(io.CalcDataIn.fire && io.CalcWeightIn.fire, calc.latency, init = False)
  when(!fireIn) {
    counterInner.clear()
    counterOuter.clear()
  }

  // fsm不一定必要,但有利于debug
  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val CALCULATING = State()

    IDLE
      .whenIsActive(when(fireIn)(goto(CALCULATING)))
    CALCULATING
      .whenIsActive(when(!fireIn)(goto(IDLE)))
  }

  io.CalcDataOut.payload.fragment := delayChain.resized // 裁剪

  // control
  io.CalcDataIn.ready := io.CalcDataOut.ready
  io.CalcWeightIn.ready := io.CalcDataOut.ready
  io.CalcDataOut.valid := counterOuter.willOverflowIfInc
  io.CalcDataOut.payload.last := counterOuter.willOverflow
}

object Calc {
  def main(args: Array[String]): Unit = {
    if (args(0) == "synth") {
      val report = VivadoFlow(
        design = new Calc(8, 8, res2_1_a),
        vivadoConfig = recommended.vivadoConfig,
        vivadoTask = VivadoTask(topModuleName = "CalcAndOut", workspacePath = "output/CalcAndOut", frequencyTarget = 600 MHz, taskType = SYNTH),
        force = true).doit()
      report.printArea
      report.printFMax
    }
    else if (args(0) == "sim") {
      val period = 2
      SimConfig.withWave.compile(new Calc(8, 8, res2_1_a))
        .doSimUntilVoid { dut =>
          fork {
            dut.clockDomain.forkStimulus(period = period)
          }
          sleep(period * 17)
          dut.io.CalcWeightIn.valid #= true
          dut.io.CalcDataIn.valid #= true
          dut.io.CalcDataOut.ready #= true
          for (i <- 0 until 10000) {
            for (j <- 0 until res2_1_a.Ih; k <- 0 until res2_1_a.Iw) {
              dut.io.CalcDataIn.payload(j)(k) #= (i + j + k) % 64
            }
            for (j <- 0 until res2_1_a.Wh; k <- 0 until res2_1_a.Ww) {
              dut.io.CalcWeightIn.payload(j)(k) #= 1
            }
            sleep(period)
          }
          simSuccess()
        }
    }
  }
}





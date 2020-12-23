package tutorial.examples

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib

import scala.util.Random

case class PixelSolverGenerics(
                                fixAmplitude: Int,
                                fixResulotion: Int,
                                iterationLimit: Int
                              ) {
  val iterationWidth = log2Up(iterationLimit + 1)
  def iterationType = UInt(iterationWidth bits)
  def fixType = SFix(
    peak = fixAmplitude exp,
    resolution = fixResulotion exp
  )
}

case class PixelTask(g: PixelSolverGenerics) extends Bundle {
  val x, y = g.fixType
}

case class PixelResult(g: PixelSolverGenerics) extends Bundle {
  val iteration = g.iterationType
}

class PixelSolver(g: PixelSolverGenerics) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (PixelTask(g))
    val rsp = master Stream (PixelResult(g))
  }

  import g._
  val x, y = Reg(fixType) init (0)
  val iteration = Reg(iterationType) init (0)

  val xx = x * x
  val yy = y * y
  val xy = x * y

  io.cmd.ready := False
  io.rsp.valid := False
  io.rsp.iteration := iteration

  when(io.cmd.valid) {
    when(xx + yy >= 4.0 || iteration === iterationLimit) {
      io.rsp.valid := True
      when(io.rsp.ready) {
        io.cmd.ready := True
        x := 0
        y := 0
        iteration := 0
      }
    }
  }.otherwise {
    x := (xx - yy + io.cmd.x).truncated
    y := (((xy) << 1) + io.cmd.y).truncated
    iteration := iteration + 1
  }
}

object PixelSolver {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new PixelSolver(PixelSolverGenerics(8, 8, 10)))
  }
}

object testPixelSolver {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new PixelSolver(PixelSolverGenerics(8, 8, 10))).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.risingEdge()
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(1)
          }
        }
        val mainThread = fork {
          dut.io.rsp.ready #= true
          dut.io.cmd.valid #= true
          for(i <- 0 until 100){
            dut.io.cmd.valid #= (if (i % 2 == 0) false else true)
            println("here")
            sleep(2)
          }
        }
        simSuccess()
      }
  }
}

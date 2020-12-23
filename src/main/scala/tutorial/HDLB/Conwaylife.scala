package tutorial.HDLB

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Conwaylife extends Component {
  val io = new Bundle {
    val clk = in Bool
    //    val reset = in Bool

    val load = in Bool
    val data = in Bits (256 bits)
    val q = out Bits (256 bits)
  }

  val clockConfig = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.load, config = clockConfig)) {
    val wrap = (x: Int) => if (x > 15) 0 else if (x < 0) 15 else x

    val qReg = RegInit(io.data)

    val matrix = Vec(Vec(Bool, 16), 16)
    val matrixNext = Vec(Vec(Bool, 16), 16)
    for(i <- 0 until 16; j <- 0 until 16) matrix(i)(j) := qReg(i * 16 + j)

    for(i <- 0 until 16; j <- 0 until 16){
      val neighbors = Bits(9 bits)
      for(k <- -1 to 1; l <- -1 to 1){
        neighbors((k+1)*3+(l+1)) := matrix(wrap(i+k))(wrap(j+l))
      }
      val sum = new ConwaySum
      sum.io.neighbors := neighbors
      sum.io.center := matrix(i)(j)
      matrixNext(i)(j) := sum.io.centerOut
    }

    for(i <- 0 until 16; j <- 0 until 16) qReg(i * 16 + j) := matrixNext(i)(j)
    io.q := qReg

    noIoPrefix()
  }
}

object Conwaylife {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Conwaylife().setDefinitionName("top_module"))
  }
}

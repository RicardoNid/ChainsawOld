package projects.FTN

import breeze.numerics.{ceil, floor}
import spinal.core._
import spinal.lib._
import spinal.core.sim._

// R + jI = (X + jY) × (C + jS), where C and S are constants while X and Y are inputs
/*
algo:
 */

class StaticComplexMultiplier(C: Double, S: Double) extends Component {

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  println(data.maxValue)
  println(data.minValue)
  println(data.resolution)

  val io = new Bundle {
//    val X, Y = in(data)
    val X, Y = in(data)
    val R, I = out(data)
  }

  val Cfix, Sfix = data
  Cfix := C
  Sfix := S

  val E = io.X - io.Y
  val Z = Cfix * E

  io.R := ((Cfix - Sfix) * io.Y + Z).truncated
  io.I := ((Cfix + Sfix) * io.X - Z).truncated
}

object StaticComplexMultiplier {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new StaticComplexMultiplier(1.25, 2.25))
  }
}

object testStaticComplexMultiplier {

  def Double2Fix(value: Double) = floor(value * (1 << 8)).toInt

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new StaticComplexMultiplier(1.25, 2.25))
      .doSimUntilVoid { dut =>

        dut.io.X.raw  #= Double2Fix(4.0)
        dut.io.Y.raw  #= 2048
        sleep(3)
        dut.io.X.raw #= 2048
        dut.io.Y.raw #= 4096
        sleep(3)
        simSuccess()
      }
  }
}

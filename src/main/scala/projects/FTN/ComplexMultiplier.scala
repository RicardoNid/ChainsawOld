package projects.FTN

import spinal.core._
import spinal.lib._

// R + jI = (X + jY) × (C + jS), where C and S are constants while X and Y are inputs
/*
algo:
 */

class StaticComplexMultiplier(C: Double, S: Double) extends Component {

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  println(data.maxValue)
  println(data.minValue)
  println(data.resolution)

  val io = new Bundle{
    val X, Y = in(data)
    val R, I = out(data)
  }

  val Cfix, Sfix = data
  Cfix := C
  Sfix := S

  val E = io.X - io.Y
  val Z = Cfix * E

  io.R := ((Cfix - Sfix) * io.Y + Z).truncated
  io.I := ((Cfix + Sfix) * io.X + Z).truncated
}

object StaticComplexMultiplier {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new StaticComplexMultiplier(1.25, 2.25))
  }
}

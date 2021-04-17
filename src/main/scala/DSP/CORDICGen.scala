package DSP

import DSP.AlgebricMode._
import DSP.RotationMode._
import spinal.core._
import spinal.lib._

case class CordicData() extends Bundle {
  val x = globalType
  val y = globalType
  val z = phaseType()
}

class CORDICGen(rotationMode: RotationMode = ROTATION,
                algebricMode: AlgebricMode = CIRCULAR) extends Component with DSPGen {

  val input = slave Flow CordicData()
  val output = master Flow CordicData()

  val config = CordicConfig(algebricMode, rotationMode)
  val cordic = CORDIC(input.payload.x, input.payload.y, input.payload.z, config)

  output.payload.x := cordic._1.truncated
  output.payload.y := cordic._2.truncated
  output.payload.z := cordic._3.truncated
  output.valid := Delay(input.valid, cordic.getDelay, init = False)
  output.valid.init(False)

  //  ComputationExtrction(output.valid)

  override def delay: Int = cordic.getDelay
}
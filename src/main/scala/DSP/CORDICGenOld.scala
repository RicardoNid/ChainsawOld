//package DSP
//
//import DSP.AlgebricMode.CIRCULAR
//import DSP.RotationMode.ROTATION
//import spinal.core._
//import spinal.lib._
//import xilinx.VivadoFlow
//
//class CORDICGen(rotationMode: RotationMode = ROTATION,
//                algebricMode: AlgebricMode = CIRCULAR,
//                arch: CORDICArch = PIPELINED,
//                iterations: Int = 11) extends Component with DSPGen {
//
//  val input = slave Flow CordicData()
//  val output = master Flow CordicData()
//
//  val outputs = CORDIC(
//    input.payload.x, input.payload.y, input.payload.z,
//    rotationMode, algebricMode,
//    iterations)
//
//  output.payload.x := outputs._1.truncated
//  output.payload.y := outputs._2.truncated
//  output.payload.z := outputs._3.truncated
//  output.valid := Delay(input.valid, iterations, init = False)
//  output.valid.init(False)
//
//  ComputationExtrction(output.valid)
//
//  override def delay: Int = iterations
//}
//
//class CORDICSin(iterations: Int) extends Component with DSPGen {
//
//  val input = slave Flow phaseType()
//  val output = master Flow unitType()
//
//  val ONE = unitType()
//  ONE := 1.0
//  val ZERO = unitType()
//  ZERO := 1.0
//
//  output.payload := CORDIC(ONE, ZERO, input.payload, ROTATION, CIRCULAR, iterations)._2.truncated
//  output.valid := Delay(input.valid, iterations, False)
//
//  override def delay: Int = iterations
//}
//
//object CORDICSin {
//  def main(args: Array[String]): Unit = {
//    val report = VivadoFlow(new CORDICSin(11), "CORDICsin", "./output/CORDICSin").doit()
//    report.printFMax
//    report.printArea
//  }
//}
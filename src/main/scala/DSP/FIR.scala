package DSP

import spinal.core._
import breeze.linalg.DenseVector

import scala.util.Random

class FIR(
           bitWidthIn: Int = 27,
           bitWidthWeight: Int = 18,
           bitWidthOut: Int = 48,
           mantissaWidth: Int = 0,
           coeffs: DenseVector[Double],
           version: String = "standard"
         ) extends Component {

  def typeIn = SFix((bitWidthIn - mantissaWidth - 1) exp, -mantissaWidth exp) // 通过这个方式实现typedef
  def typeWeight = SFix((bitWidthWeight - mantissaWidth - 1) exp, -mantissaWidth exp)

  def typeMult = SFix((bitWidthIn + bitWidthWeight - mantissaWidth - 1) exp, -mantissaWidth exp) // ?
  def typeOut = SFix((bitWidthOut - mantissaWidth - 1) exp, -mantissaWidth exp)

  val io = new Bundle {
    val dataIn = in SFix((bitWidthIn - mantissaWidth - 1) exp, -mantissaWidth exp)
    val dataOut = out SFix((bitWidthOut - mantissaWidth - 1) exp, -mantissaWidth exp)
  }

  val length = coeffs.length

  val weightWires = Vec(typeWeight, length)
  for (i <- 0 until length) weightWires(i) := coeffs(i)

  val inputZERO = typeIn
  inputZERO := 0 // 如果写成0.0,ZERO的位数会被拓展

  if (version == "systolic") {
    //    val inReg = RegInit(inputZERO)
    //    val xRegs = RegInit(Vec(Seq.fill(2 * length - 1)(inputZERO)))
    // fixme : spinal会把reset全部放在一起处理,"打断"了综合器对于代码的阅读,而无法推断出相邻DSP间的传播,无法推断出脉动阵列
    // fixme : 需要显示reset,而非使用RegInit
    val inReg = RegNext(io.dataIn) // fixme : 如果不使用inReg,xRegs(0) := io.in 将会大大增加时延 找出原因
    val xRegs = Reg(Vec(typeIn, 2 * length - 1))
    val multRegs = Reg(Vec(typeMult, length))
    val yRegs = Reg(Vec(typeOut, length))

    //    inReg := io.dataIn // fixme : 如果不使用inReg,xRegs(0) := io.in 将会大大增加时延 找出原因
    //    xRegs(0) := inReg
    //    for (i <- 1 until xRegs.length) xRegs(i) := xRegs(i - 1) // 连接输入传播线
    //    for (i <- 0 until length) multRegs(i) := weightWires(i) * xRegs(i * 2)
    //    yRegs(0) := multRegs(0)
    //    for (i <- 1 until length) yRegs(i) := yRegs(i - 1) + multRegs(i)
    //    //    io.out := yRegs(length - 1)
    //    val bufReg = RegNext(yRegs(length - 1))
    //    io.dataOut := bufReg

    xRegs(0) := inReg
    multRegs(0) := weightWires(0) * xRegs(0)
    yRegs(0) := multRegs(0)
    for (i <- 1 until length) {
      xRegs(2 * i - 1) := xRegs(2 * i - 2)
      xRegs(2 * i) := xRegs(2 * i - 1)
      multRegs(i) := weightWires(i) * xRegs(2 * i)
      yRegs(i) := yRegs(i - 1) + multRegs(i)
    }
    val bufReg = RegNext(yRegs(length - 1))
    io.dataOut := bufReg

  }
}

object FIR {
  def main(args: Array[String]): Unit = {

    val randGen = new Random(30)
    val coeff144 = DenseVector(Array.ofDim[Double](144).map(_ => {
      val value = randGen.nextInt % 100 + 500 // 避开较小的整数和2的幂,避免优化
      if (isPow2(value)) (value + 13).toDouble
      else value.toDouble
    }))

    SpinalVerilog(new FIR(coeffs = coeff144, version = "systolic"))
  }
}

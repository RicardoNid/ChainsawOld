package sysu.FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import sysu.FTN.MultiChanFIR.coeffTable

import scala.util.Random

case class ComplexNumberVector(bitWidth: Int, num: Int, direction: IODirection) extends Bundle {
  val real = direction Vec(SInt(bitWidth bits), num)
  val imag = direction Vec(SInt(bitWidth bits), num)
}

case class MultiChanFIRConfig(
                               widthInput: Int = 8,
                               widthCoeff: Int = 8,
                               widthOutput: Int = 8,
                               numCarrier: Int = 64, // 子载波数量
                               numParallelCarrier: Int = 64, // 并行子载波数量
                               numTap: Int = 8, // 系数/抽头数量
                               numParallelTap: Int = 8 // 并行抽头数量
                             ) {
  require(numCarrier % numParallelCarrier == 0)
  require(numTap % numParallelTap == 0)
}

class MultiChanFIR(config: MultiChanFIRConfig) extends Component {

  val io = new Bundle {
    val input = ComplexNumberVector(config.widthInput, config.numParallelCarrier, in)
    val output = ComplexNumberVector(config.widthOutput, config.numParallelCarrier, out)
  }

  def typeIn = SInt(config.widthInput bits)

  def typeCoeff = SInt(config.widthCoeff bits)

  def typeOut = SInt(config.widthOutput bits)

  //  val coeffROMs = Range(0, config.numParallelCarrier).map(_ => Mem(SInt(config.widthCoeff bits), coeffTable))

  // 完全展开,coeff固化在DSP连线上
  if (config.numCarrier == config.numParallelCarrier && config.numTap == config.numParallelTap) {
    // 输入线
    val xRealCols = Range(0, config.numCarrier).map(_ => Reg(Vec(typeIn, config.numTap)))
    val xImagCols = Range(0, config.numCarrier).map(_ => Reg(Vec(typeIn, config.numTap)))
    Range(0, config.numCarrier).foreach(indexCarrier => xRealCols(indexCarrier)(0) := io.input.real(indexCarrier))
    Range(0, config.numCarrier).foreach(indexCarrier => xImagCols(indexCarrier)(0) := io.input.imag(indexCarrier))
    xRealCols.foreach(col => for (i <- 1 until config.numTap) col(i) := col(i - 1))
    xImagCols.foreach(col => for (i <- 1 until config.numTap) col(i) := col(i - 1))
    // 标准FIR,超长的关键路径
    val resultRealRow = Reg(Vec(typeOut, config.numCarrier))
    val resultImagRow = Reg(Vec(typeOut, config.numCarrier))
    Range(0, config.numCarrier).foreach(indexCarrier => {
      resultRealRow(indexCarrier) := Range(0, config.numTap)
        .map(indexTap => xRealCols(indexCarrier)(indexTap) * coeffTable(indexCarrier, indexTap)).reduce(_ + _).resized
      resultImagRow(indexCarrier) := Range(0, config.numTap)
        .map(indexTap => xImagCols(indexCarrier)(indexTap) * coeffTable(indexCarrier, indexTap)).reduce(_ + _).resized
    })
    // 输出
    Range(0, config.numCarrier).foreach(indexCarrier => io.output.real(indexCarrier) := resultRealRow(indexCarrier))
    Range(0, config.numCarrier).foreach(indexCarrier => io.output.imag(indexCarrier) := resultImagRow(indexCarrier))
  }
}

object MultiChanFIR {

  def coeffTable(indexCarrier: Int, indexTap: Int) = S(1758)

  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new MultiChanFIR(MultiChanFIRConfig()))
  }
}

object testMultiChanFIR {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new MultiChanFIR(new MultiChanFIRConfig())).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.risingEdge()
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(1)
          }
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}

package sysu.CNN

import breeze.linalg._
import spinal.core._
import spinal.core.sim._
import sysu.util._
import sysu.xilinx._

// generate input seqGen,BRAM,and output seqGen according to the addrSeq
class MemWizard(bitWidth: Int,
                BRAMAddrSeq: DenseMatrix[Int]) extends Component {

  def data = UInt(bitWidth bits)

  val io = new Bundle {
    val output = out Vec(data, BRAMAddrSeq.cols)
  }

  val cols = getColArrays(BRAMAddrSeq)

  val depth = cols.map(_.distinct.length).max
  val BRAMS = (0 until BRAMAddrSeq.cols).map(i => Mem(data, cols(i).map(U(_))))

  val seqGens = (0 until BRAMAddrSeq.cols).map(i => new SeqGen(cols(i)))

  (0 until BRAMAddrSeq.cols).foreach(i => io.output(i) := BRAMS(i).readSync(seqGens(i).io.output))
}

object MemWizard {

  val seq = addrSeq(3, 7, 7, 3, 3, 3, true) // for full analysis
  //  val seq = inputMapRowAddrSeq(256, 16, 16, 3, 3, 3, true) // vgg11的第四层

  //  val seq = DenseMatrix.tabulate(256,9){case(i, j) => i}

  def main(args: Array[String]): Unit = {
    val moduleName = "BRAMWizard"
    val flow = VivadoFlow(
      design = new MemWizard(8, seq),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        //        workspacePath = "output/CNN/BRAMWizard_256_16_16_3_3_3"),
        workspacePath = "output/CNN/BRAMWizard_3_7_7_3_3_3"),
      force = true
    )
    val report = flow.doit()
    report.printFMax
    report.printArea
  }
}

object testBRAMWizard {

  val period = 2

  val seq = addrSeq(5, 7, 7, 3, 3, 3, true)

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new MemWizard(8, seq)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}

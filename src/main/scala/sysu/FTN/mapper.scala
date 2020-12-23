package sysu.FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class mapper(bitWidth: Int = 16,
             numSubcarrier: Int = 64,
             numFreqCompo: Int = 3,
             numTimeCompo: Int = 3) extends Component {

  val io = new Bundle {
    val input = Vec(UInt(bitWidth bits), numSubcarrier)
    val otuput = Vec(UInt(bitWidth bits), numSubcarrier)
  }

  // 输入延迟线
  val inputRegs = Reg( Vec(Vec(UInt(bitWidth bits), numTimeCompo), numSubcarrier))

}

object mapper {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new mapper)
  }
}

object testmapper {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new mapper).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}

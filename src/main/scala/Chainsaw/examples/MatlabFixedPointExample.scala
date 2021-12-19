package Chainsaw.examples

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

/** this example shows how to verify a DUT by matlab fixed point model
 *
 */
object MatlabFixedPointExample {
  def main(args: Array[String]): Unit = {
    eng.setWorkingDir("~/FTN326/fixedPoint")
    eng.eval("testFixedFFT")
    val data = eng.load[Array[BComplex]]("fixedFFT", "fftInput")
    val output = eng.load[Array[BComplex]]("fixedFFT", "fftOutput")
    println(data.mkString(" "))
    println(output.mkString(" "))

    import Chainsaw.DSP.FFT.CooleyTukeyFFT

    SimConfig.withWave.compile(
      CooleyTukeyFFT(64, Seq(2,2,2,2,2,2), inverse = false, SFix(1 exp, -6 exp), SFix(1 exp, -6 exp)))
      .doSim{dut =>

        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitSampling()

        dut.dataIn.payload.zip(data).foreach{ case (port, data) =>
          port.real.raw #= data.real.toInt
          port.imag.raw #= data.imag.toInt
        }

        dut.clockDomain.waitSampling(30)
    }
  }
}

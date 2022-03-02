package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.algos.Qam.{getRms, getSymbols}
import breeze.numerics._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
class AdaptiveQammodTest extends AnyFlatSpec {

  "adaptiveQammod" should "work correctly" in {
    val sfix = HardType(SFix(2 exp, -15 exp))

    SimConfig.withFstWave.compile(AdaptiveQammod(Seq(2, 4, 6), sfix)).doSim { dut =>

      dut.clockDomain.forkStimulus(2)

      def testForOnce(alloc: Int) = {
        dut.orderIn #= (alloc - 1) // modulation order = 1 << 2 = 4
        val bitsIn = ChainsawRand.nextInt(1 << alloc)
        dut.bitsIn #= bitsIn // first symbol
        dut.clockDomain.waitSampling(3) // 2 + 1
        val golden = getSymbols(1 << alloc)(bitsIn) / getRms(1 << alloc)
        val yours = dut.symbolOut.toComplex
        assert(abs(golden - yours) < 1E-2)
      }

      (0 until 100).foreach(_ => testForOnce(6))
      (0 until 100).foreach(_ => testForOnce(4))
      (0 until 100).foreach(_ => testForOnce(2))

    }
  }

}

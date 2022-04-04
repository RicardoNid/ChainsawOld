package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.sim._
import scala.util.Random
import Chainsaw._
import Para._
import scala.math._

class UnwrapTest extends AnyFlatSpec {
  "The result" should "meet a the standard \"|x(n)' - x(n - 1)| <= 1 and |x(n)' - x(n)| % 2 = 0\"" in
    SimConfig.withFstWave.compile(new Unwrap()).doSim { dut =>
      import dut._
      Range(0, 1000).foreach { i =>
        io.xn.raw  #= ChainsawRand.nextBigInt(peakExp + resolutionExp - 1)
        io.xn1.raw #= ChainsawRand.nextBigInt(peakExp + resolutionExp - 1)
        sleep(1)
        // |x(n)' - x(n - 1)| <= 1 and |x(n)' - x(n)| % 2 = 0
        assert(abs(io.xn1.toDouble - io.xnOut.toDouble) <= 1 && abs(io.xnOut.toDouble - io.xn.toDouble).toInt % 2 == 0)
      }
    }
}

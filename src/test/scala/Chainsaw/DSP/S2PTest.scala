package Chainsaw.DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class S2PTest extends AnyFlatSpec {

  behavior of "S2PTest"

  def testP2S(p: Int, s: Int) = {
    val testCases = (0 until 10 * p / s).map(_ => (0 until s).map(_ => ChainsawRand.nextBigInt(8)))
    val goldens: Seq[Seq[BigInt]] = testCases.grouped(p / s).toSeq.map(segments => segments.reduce(_ ++ _))
    doFlowPeekPokeTest(s"testS2Pat${s}to${p}", S2P(s, p, HardType(UInt(8 bits))), testCases, goldens)
  }

  it should "apply" in {
    val s = 32
    (0 until 10).foreach(_ => testP2S(ChainsawRand.nextInt(20) * s, s))
  }
}

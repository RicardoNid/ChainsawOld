package Chainsaw.DSP

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class P2STest extends AnyFlatSpec {

  behavior of "P2STest"

  def testP2S(p: Int, s: Int) = {
    val testCases = (0 until 10).map(_ => (0 until p).map(_ => ChainsawRand.nextBigInt(8)))
    val goldens: Seq[Seq[BigInt]] = testCases.flatMap(_.grouped(s).toSeq)
    doFlowPeekPokeTest(s"testP2Sat${p}to${s}", P2S(p, s, HardType(UInt(8 bits))), testCases, goldens)
  }

  it should "apply" in {
    val s = 32
    (0 until 10).foreach(_ => testP2S(ChainsawRand.nextInt(20) * s, s))
  }
}

package Chainsaw

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

class ComplexMultTest extends AnyFlatSpec {

  val aForTest = toComplexType(HardType(SFix(6 exp ,-11 exp))) // 18 bits
  val bForTest = toComplexType(HardType(SFix(2 exp ,-15 exp))) // 18 bits

  "ComplexMult" should "consume 3 DSPs and no LUTs" in{
    val reportFast3 = VivadoSynth(ComplexMult(aForTest, bForTest)(ComplexMultConfig(fast = true, 3)), "pipeline3cMult")
    assert(reportFast3.LUT == 0 && reportFast3.DSP == 3)
    val reportFast4 = VivadoSynth(ComplexMult(aForTest, bForTest)(ComplexMultConfig(fast = true, 4)), "pipeline6cMult")
    assert(reportFast4.LUT == 0 && reportFast4.DSP == 3)
    val reportFast6 = VivadoSynth(ComplexMult(aForTest, bForTest)(ComplexMultConfig(fast = true, 6)), "pipeline6cMult")
    assert(reportFast6.LUT == 0 && reportFast6.DSP == 3)
  }

  // TODO:
  it should "work correctly" in{

  }

}

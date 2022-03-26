package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import Chainsaw.DSP.DAS._

class QuartusFlowTest extends AnyFlatSpec {

  it should "synthesis and report" in {
    val quartusFlow = new QuartusFlow(DivideCordic()).impl()
  }
}

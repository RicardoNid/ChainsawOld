package Chainsaw.crypto.symmetric

import Chainsaw.DFG.{ImplPolicy, genDFG, globalImplPolicy, synthImplPolicy, testImplPolicy}
import Chainsaw.{GenRTL, VivadoSynth}
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class SM4Test extends AnyFlatSpec {

  import SM4._

  val doSynth = false

  globalImplPolicy = synthImplPolicy

  "the SM4 design" should "generate correctly" in {
    GenRTL(SM4HardwareDFG(baseLineConfig))
  }

  "the SM4 design" should "work correctly on combinational version" in {
    val combConfig = SM4Config(readAsync = true, usingBRAM = false, usingROM = true, 1, comb = true)
    doFlowPeekPokeTest(s"testSM4_combinational", SM4HardwareDFG(combConfig), Seq.fill(baseLineConfig.parallelism.abs)(Seq(testCase, testKey)), Seq(golden))
  }

  "the SM4 design" should "work correctly on baseline config" in {
    doFlowPeekPokeTest(
      s"testSM4_$baseLineConfig",
      SM4HardwareDFG(baseLineConfig),
      Seq.fill(baseLineConfig.parallelism.abs)(Seq(testCase, testKey)),
      Seq(golden)
    )
  }

  "the SM4 design" should "work correctly on all configs" in {

    val readAsyncConfigs = Seq(true, false)
    val usingBRAMConfigs = Seq(true, false)
    //    val parallelismConfigs = Seq(1, -2, -4)
    val parallelismConfigs = Seq(1)

    //    Seq.tabulate(2, 2, 3) { (i, j, k) =>
    Seq.tabulate(2, 2, 1) { (i, j, k) =>
      val config = SM4Config(readAsync = readAsyncConfigs(i), usingBRAM = usingBRAMConfigs(j), usingROM = true, parallelism = parallelismConfigs(k))
      doFlowPeekPokeTest(s"testSM4_$config", new SM4HardwareDFG(config = config), Seq.fill(config.parallelism.abs)(Seq(testCase, testKey)), Seq(golden))
      if (doSynth) VivadoSynth(new SM4HardwareDFG(config), s"synthSM4_$config")
    }
  }

  globalImplPolicy = testImplPolicy
}

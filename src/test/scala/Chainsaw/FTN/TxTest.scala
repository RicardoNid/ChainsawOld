package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.apache.commons.io.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._

import java.io.File
import scala.collection.mutable.ArrayBuffer

class TxTest extends AnyFlatSpec {

  val testSize = 4
  prepareAlloc("3_226_N15")
  implicit val params: FtnParams = FtnParams(2, 256, doBitAlloc = true, useSyncData = false, useGivenAlloc = true)
  //  implicit val params: FtnParams = FtnParams(2, 256, doBitAlloc = true)

  import params._

  "Tx" should "work correctly as a Whole with DataGen and Packing" in {

    forEachParams { params =>
      SimConfig.withFstWave.workspaceName(s"TxOnBoard${params.start}_${params.end}_${params.doBitAlloc}")
        .compile(TxOnBoard()(params)).doSim { dut =>
        val results = ArrayBuffer[BigInt]()
        dut.clockDomain.forkStimulus(2)
        dut.clockDomain.waitSampling()
        dut.dataOut.setMonitor(results)
        dut.dataOut.ready #= true
        dut.clockDomain.waitSampling(500)

        val dataString = results.take(params.txPacked.length).map(bits =>
          bits.toString(2).padToLeft(768, '0').grouped(6).toSeq
            .map(bits => BigInt(bits.mkString(""), 2).toInt).reverse)
          .padTo(80, Seq.fill(128)(32))

        results.zip(params.txPackedAll).foreach { case (yours, golden) =>
          val yourPacked: Seq[BigInt] = yours.toString(2).padToLeft(768, '0').grouped(6).toSeq
            .map(bits => BigInt(bits.mkString(""), 2))
            .reverse
          println(s"yours : ${yourPacked.mkString(" ")}")
          println(s"golden: ${golden.mkString(" ")}")
          assert(yourPacked.zip(golden).map { case (your, gold) => (your - gold).abs }.forall(_ <= 1))
        }
      }
    }
  }

  it should "work correctly on convenc and interleave" in {
    doFlowPeekPokeTest(
      name = "testTx0", dut = Tx0(),
      testCases = Seq.fill(testSize)(txRaw).flatten,
      golden = Seq.fill(testSize)(txInterleaved).flatten,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on qammod" in {
    doFlowPeekPokeTest(
      name = "testTx1", dut = Tx1(),
      testCases = Seq.fill(testSize)(txRaw).flatten,
      golden = Seq.fill(testSize)(txMapped).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-3
    )
  }

  it should "work correctly on ifft" in {
    doFlowPeekPokeTest(
      name = "testTx", dut = Tx2(),
      testCases = Seq.fill(testSize)(txRaw).flatten,
      golden = Seq.fill(testSize)(txModulated).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 2E0
    )
  }

  it should "work correctly as a Whole" in {
    forEachParams { params =>
      doFlowPeekPokeTest(
        name = "testTx", dut = TxWhole()(params),
        testCases = Seq.fill(testSize)(params.txRaw).flatten,
        golden = Seq.fill(testSize)(params.txScaled).flatten,
        testMetric = TestMetric.APPROXIMATE, epsilon = 1
      )
    }
  }


  it should "synth for all components" in {
    //    VivadoSynthForTiming(Convenc128FTN(), "convencTx")
    //    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool())), "interleaveTx")
    //    VivadoSynthForTiming(comm.qam.AdaptiveQammod(bitAlloc, powAlloc, symbolType), "qammodTx")
    //    VivadoSynthForTiming(DSP.FFT.CooleyTukeyHSIFFT(512, 128, ifftType, symbolType, Seq(4, 4, 4, 4, 2), ifftShifts), "ifftTx")
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyHSIFFT(512, 128, ifftType, symbolType, Seq(8, 8, 4, 2), ifftShifts), "ifftTx")
    //    VivadoSynthForTiming(TxPacking(), "txPacking")
    //    VivadoSynthForTiming(TxWhole(), "TxWhole")
    //    VivadoSynth(TxOnBoard(), "TxOnBoard")
  }
}

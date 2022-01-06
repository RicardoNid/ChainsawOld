package Chainsaw.DSP.interleave

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

class MatIntrlvTest extends AnyFlatSpec {

  behavior of "MatIntrlvTest"

  def testMatIntrlv(testSize: Int, row: Int, col: Int) = {

    val dataType = HardType(UInt(log2Up(row * col) bits))
    val testCases: Seq[Seq[Seq[BigInt]]] = (0 until testSize).map { _ =>
      Seq.tabulate(row, col)((_, _) => ChainsawRand.nextBigInt(log2Up(row * col)))
    }

    val goldens = testCases.map { matrix =>
      val row = matrix.size
      val col = matrix.head.size
      Seq.tabulate(col, row)((i, j) => matrix(j)(i)) // transpose
    }

    doFlowPeekPokeTest(
      name = s"testMatintrlv$row*$col",
      MatIntrlv(row, col, dataType),
      testCases = testCases.flatten,
      golden = goldens.flatten
    )
  }

  it should "work for different sizes" in {
    def testRand() =
      testMatIntrlv(10, ChainsawRand.nextInt(20), ChainsawRand.nextInt(20))
    (0 until 10).foreach(_ => testRand())
  }

  it should "synth" in {
    VivadoSynth(
      new Component with DSPTestable[Vec[UInt], Vec[UInt]] {
        val core = MatIntrlv(128, 128, HardType(UInt(18 bits)))
        override val dataIn = slave(cloneOf(core.dataIn))
        override val dataOut = master(cloneOf(core.dataOut))
        override val latency = core.latency + 2
        dataIn.m2sPipe() >> core.dataIn
        core.dataOut.m2sPipe() >> dataOut
      }
    )
  }

  it should "synth for FTN" in {
    VivadoSynth(
      new Component with DSPTestable[Vec[Bits], Vec[Bits]] {
        val core = AdaptiveMatIntrlv(256, 512, 512, 512, HardType(Bits(1 bits)))
        override val dataIn = slave(cloneOf(core.dataIn))
        override val dataOut = master(cloneOf(core.dataOut))
        override val latency = core.latency + 2
        dataIn.m2sPipe() >> core.dataIn
        core.dataOut.m2sPipe() >> dataOut
      }
    )

    VivadoSynth(AdaptiveMatIntrlv(256, 512, 512, 512, HardType(Bits(1 bits))))
  }
}

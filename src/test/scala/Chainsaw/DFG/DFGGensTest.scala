package Chainsaw.DFG

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import Chainsaw.DFG.Operators._

import scala.language.postfixOps

class DFGGensTest extends AnyFlatSpec {

  import FirArch._

  val add = BinaryNode(sintAdd, "add")
  val mult = BinaryNode(sintMult, "mult")
  def multAdd(delay: Int) = TrinaryNode(sintMACDSP(delay), "multAdd", delay = delay cycles)

  val coeffs = (0 until 10).map(_ => DSPRand.nextInt(1 << 16) + 1000)
  val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(2))
  val golden = eng.feval("filter", coeffs.reverse.map(_.toDouble).toArray, Array(1), testCase.map(_.toDouble).toArray).asInstanceOf[Array[Double]]

  val dut = (arch: FirArch, useDSP: Boolean, delay: Int) => new Component with DSPTestable[SInt, SInt] {
    val dataIn: Flow[SInt] = slave Flow SInt(18 bits)
    val dataOut: Flow[SInt] = master Flow SInt()
    val latency = if (arch == SYSTOLIC) coeffs.size - 1 else 0 + (if (useDSP) delay else 0)
    val firDFG = if (useDSP) DFGGens.firDSP(multAdd(delay), arch, coeffs, 18 bits)
    else DFGGens.fir(add, mult, arch, coeffs, 18 bits)
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
    dataOut.payload := firDFG.impl(Seq(dataIn.payload)).head
    //    dataOut.payload := RegNext(firDFG.impl(Seq(RegNext(dataIn.payload))).head) // for timing analysis
  }

  def testFIR(arch: FirArch, useDSP: Boolean) =
    doFlowPeekPokeTest(dut(arch, useDSP, 1), s"testFir${arch}${if (useDSP) "_dsp" else ""}", testCase, golden.map(_.toInt))

  "fir structure" should "be correct as direct fir" in testFIR(DIRECT, false)
  it should "be correct as transpose fir" in testFIR(TRANSPOSE, false)
  it should "be correct as systolic fir" in testFIR(SYSTOLIC, false)
  it should "be correct as direct fir using dsp slice" in testFIR(DIRECT, true)

  it should "be implemented efficiently as by dsp slices" in {
    VivadoSynth(dut(DIRECT, true, 0))
    VivadoSynth(dut(TRANSPOSE, true, 0))
    VivadoSynth(dut(DIRECT, true, 1))
    VivadoSynth(dut(TRANSPOSE, true, 1))
  }

  "fir structure" should "be correct as convenc" in {

    val and = BinaryNode(Operators.and, "and")
    val xor = BinaryNode(Operators.xor, "xor")

    def convDirect(coeffs: Seq[Int]): DFGGraph[Bits] = DFGGens.fir(xor, and, DIRECT, coeffs, 1 bits)

    import Communication.channelCoding._
    val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(1))
    val conv802_11: ConvConfig = ConvConfig(Array(171, 133), radix = 8)
    val trellisM = Refs.poly2trellisM(conv802_11.ms.map(_ + 1), conv802_11.codeGens)
    val golden: Array[Int] = Refs.convenc(testCase.map(_.toInt).toArray, trellisM)

    doFlowPeekPokeTest(new Component with DSPTestable[Vec[Bits], Vec[Bits]] {
      val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(1 bits), 1)
      val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), 2)
      val latency = 0
      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload(0) := convDirect(BigInt("171", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)).impl(Seq(dataIn.payload).head).head
      dataOut.payload(1) := convDirect(BigInt("133", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)).impl(Seq(dataIn.payload).head).head
    }, "testConv", testCase.grouped(1).toSeq, golden)
  }
}

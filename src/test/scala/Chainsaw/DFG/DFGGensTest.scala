package Chainsaw.DFG

import Chainsaw.DFG._
import Chainsaw.DFG.FFTArch._
import Chainsaw.DFG.Operators._
import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import cc.redberry.rings.scaladsl._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class DFGGensTest extends AnyFlatSpec {

  import FirArch._

  val add: BinaryNode[SInt] = BinaryNode(sintAdd, "add")
  val mult: BinaryNode[SInt] = BinaryNode(sintMult, "mult")

  def multAdd(delay: Int) = TrinaryNode(sintMACDSP(delay), "multAdd", delay = delay cycles)

  val coeffs = (0 until 10).map(_ => DSPRand.nextInt(1 << 16) + 1000)
  val firTestCase = (0 until 100).map(_ => DSPRand.nextBigInt(2))
  val firGolden = eng.feval("filter", coeffs.reverse.map(_.toDouble).toArray, Array(1), firTestCase.map(_.toDouble).toArray).asInstanceOf[Array[Double]]

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
    doFlowPeekPokeTest(s"testFir$arch${if (useDSP) "_dsp" else ""}", dut(arch, useDSP, 1), firTestCase, firGolden.map(_.toInt))

  "fir structure" should "be correct as direct fir" in testFIR(DIRECT, useDSP = false)
  it should "be correct as transpose fir" in testFIR(TRANSPOSE, useDSP = false)
  it should "be correct as systolic fir" in testFIR(SYSTOLIC, useDSP = false)
  it should "be correct as direct fir using dsp slice" in testFIR(DIRECT, useDSP = true)

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

    doFlowPeekPokeTest("testConv", new Component with DSPTestable[Vec[Bits], Vec[Bits]] {
      val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(1 bits), 1)
      val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), 2)
      val latency = 0
      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload(0) := convDirect(BigInt("171", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)).impl(Seq(dataIn.payload).head).head
      dataOut.payload(1) := convDirect(BigInt("133", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)).impl(Seq(dataIn.payload).head).head
    }, testCase.grouped(1).toSeq, golden)
  }

  "fft structure" should "be correct as fft" in {

  }

  import crypto._
  import crypto.lattice.HuaweiKyber._

  val N = 128
  val nttAlgo = crypto.NTT(cfRing, N)
  val gsbf: ButterflyNode[Long] = ButterflyNode(nttAlgo.gsButterfly(_, _, _), "gsButterfly")
  val ctbf: ButterflyNode[Long] = ButterflyNode(nttAlgo.ctButterfly(_, _, _), "ctButterfly")
  val bitReverse: Seq[Long] => Seq[Long] = nttAlgo.bitReverse(_)

  val omega = cfRing.getNthRoot(N)
  val inverseN = cfRing.pow(N, -1)
  val coeffGen = (index: Int) => cfRing.pow(omega, index)
  val nttTestCase = (0 until N).map(_ => DSPRand.nextInt(p).toLong)
  val nttGolden = nttAlgo.NTT(nttTestCase)

  implicit def converter(value: Long, width: BitCount) = value

  "fft structure" should "be correct as ntt" in {
    val nttDFG = DFGGens.radix2fft(ctbf, gsbf, N, DIF, inverse = false, coeffGen, -1 bits)
    val nttImpl = new DFGImplSoft[Long](nttDFG)(0).implForwarding
    val nttYours = nttImpl(nttTestCase)
    logger.info(s"DFG    result:\n${nttYours.sorted.mkString(" ")}")
    logger.info(s"golden result:\n${nttGolden.sorted.mkString(" ")}")
    assert(nttYours.diff(nttGolden).isEmpty)
  }

  it should "be correct as intt" in {
    val inttDFG = DFGGens.radix2fft(ctbf, gsbf, N, DIT, inverse = true, coeffGen, -1 bits)
    val inttImpl = new DFGImplSoft[Long](inttDFG)(0).implForwarding
    assert(inttImpl(bitReverse(nttGolden)).map(value => cfRing(value * inverseN)).diff(nttTestCase).isEmpty)
  }

  // testing hardware DFG
  val gsbfHard = crypto.lattice.HuaweiKyber.gsButterflyNode
  val ctbfHard = crypto.lattice.HuaweiKyber.ctButterflyNode
  val k2Inverse = cfRing.inverseOf(k2)

  val coeffGenHard = (index: Int) => cfRing(cfRing.pow(omega, index) * k2Inverse)
  val knttTestCase = nttTestCase.map(value => cfRing(value * k2Inverse))
  val knttGolden = nttGolden.map(value => cfRing(value * k2Inverse))

  implicit def long2Uint: (Long, BitCount) => UInt = (value: Long, width: BitCount) => U(value, 12 bits) // TODO:

  it should "be correct as NTT hardware" in {
    val nttDFG = DFGGens.radix2fft(ctbfHard, gsbfHard, N, DIF, inverse = false, coeffGenHard, 12 bits)

    testDSPNode[UInt, Seq[BigInt], BigInt](
      nttDFG.asNode("ntt", log2Up(N) * 5 cycles), Seq.fill(N)(12 bits),
      Seq(knttTestCase.map(BigInt(_))),
      knttGolden.map(BigInt(_)))

    synthDSPNode[UInt](nttDFG.asNode("ntt", log2Up(N) * 5 cycles), Seq.fill(N)(12 bits))
  }

}

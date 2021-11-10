package Chainsaw.DFG

import Chainsaw.DFG.FFTArch._
import Chainsaw.DFG.Operators._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import Chainsaw.{crypto, _}
import cc.redberry.rings.scaladsl._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import xilinx.VivadoReport

import scala.language.postfixOps

// TODO: verify folding of following tests
// TODO: fft as fft itself

/** Tests below shows that DFGGens functions are abstract "architectures" which can be used in implementation of different specific designs
 * as for the ftt as ntt hardware example
 *
 * @see [[crypto.lattice.HuaweiKyberTest]]
 */
class DFGGensTest extends AnyFlatSpec {

  import FirArch._

  val doSynths: Boolean = false

  // operators for fir test
  val add: BinaryNode[SInt] = BinaryNode("add", sintAdd)
  val mult: BinaryNode[SInt] = BinaryNode("mult", sintMult)


  def dspMAC: TrinaryNode[SInt] = Operators.macDSP48(mreg = true)

  val firSize = 10
  val firCoeffs: Seq[Int] = (0 until firSize).map(_ => DSPRand.nextInt(1 << 16) + 1000)
  val firTestCase: Seq[BigInt] = Seq.fill(firSize)(BigInt(0)) ++ (0 until 100).map(_ => DSPRand.nextBigInt(2))
  // generate golden by Matlab
  val firGolden: Array[Double] = eng.feval("filter", firCoeffs.reverse.map(_.toDouble).toArray, Array(1), firTestCase.map(_.toDouble).toArray).asInstanceOf[Array[Double]]

  def firDUT(arch: FirArch, useDSP: Boolean): DSPNode[SInt] =
    if (useDSP) FIRGen(dspMAC, arch, firCoeffs, 18 bits, 1).getGraphAsNode()
    else FIRGen(add, mult, arch, firCoeffs, 18 bits, 1).getGraphAsNode()

  def testFIR(arch: FirArch, useDSP: Boolean): Seq[Seq[BigInt]] =
    testDSPNode[SInt, Seq[BigInt], Int](firDUT(arch, useDSP), Seq(18 bits), firTestCase.map(Seq(_)), firGolden.map(_.toInt), initLength = firSize)

  def synthFIR(arch: FirArch, useDSP: Boolean): VivadoReport =
    synthDSPNode[SInt](firDUT(arch, useDSP), Seq(18 bits))

  // implement fir(real field) by fir DFG
  behavior of "fir DFG as fir"

  "fir" should "be correct as direct fir" in testFIR(DIRECT, useDSP = false)
  it should "be correct as transpose fir" in testFIR(TRANSPOSE, useDSP = false)
  it should "be correct as systolic fir" in testFIR(SYSTOLIC, useDSP = false)

  it should "be correct as direct fir using dsp slice" in testFIR(DIRECT, useDSP = true)
  it should "be correct as transpose fir using dsp slice" in testFIR(TRANSPOSE, useDSP = true)
  it should "be correct as systolic fir using dsp slice" in testFIR(SYSTOLIC, useDSP = true)

  if (doSynths) {
    it should "be implemented efficiently as by dsp slices" in {
      synthFIR(DIRECT, useDSP = true)
      synthFIR(TRANSPOSE, useDSP = true)
      synthFIR(SYSTOLIC, useDSP = true)
    }
  }

  behavior of "fir DFG as convenc"

  // implement convolutional encoder(finite field) by fir DFG
  "fir structure" should "be correct as convenc" in {
    // operators for convenc test
    val and = BinaryNode("and",Operators.and )
    val xor = BinaryNode("xor",Operators.xor )

    import comm.channelCoding._
    val convencTestCase = Seq.fill(7)(BigInt(0)) ++ (0 until 100).map(_ => DSPRand.nextBigInt(1))
    val conv802_11: ConvConfig = ConvConfig(Array(171, 133), radix = 8)
    val trellisM = Refs.poly2trellisM(conv802_11.ms.map(_ + 1), conv802_11.codeGens)
    // generate golden by Matlab
    val convencGolden: Array[Int] = Refs.convenc(convencTestCase.map(_.toInt).toArray, trellisM)

    val coeff171 = BigInt("171", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)
    val coeff133 = BigInt("133", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)

    def convencGen(coeffs: Seq[Int], firArch: FirArch): FIRGen[Bits, Int] = FIRGen(xor, and, firArch, coeffs, 1 bits, 1)

    def testConvenc(firArch: FirArch) = {
      doFlowPeekPokeTest("testConv", new Component with DSPTestable[Vec[Bits], Vec[Bits]] {
        val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(1 bits), 1)
        val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), 2)
        val latency = convencGen(coeff171, firArch).latency
        dataOut.valid := Delay(dataIn.valid, latency, init = False)
        dataOut.payload(0) := convencGen(coeff171, firArch).getGraph.impl(Seq(dataIn.payload).head).head
        dataOut.payload(1) := convencGen(coeff133, firArch).getGraph.impl(Seq(dataIn.payload).head).head
      }, convencTestCase.grouped(1).toSeq, convencGolden, initLength = 7)
    }

    testConvenc(DIRECT)
    testConvenc(TRANSPOSE)
  }

  behavior of "radix-2 fft DFG(butterfly) as fft"

  "fft structure" should "be correct as fft" taggedAs (DFGTest) in {

  }

  behavior of "radix-2 fft DFG(butterfly) as ntt"

  // this is deprecated as we change our mind and do not allow DFG tobe implemented as software

  //  import crypto._
  //  import crypto.lattice.HuaweiKyber._ // using parameters of CRYSTALS-KYBER
  //
  //  val N = 128
  //  val nttAlgo: NTT = crypto.NTT(cfRing, N)
  //
  //  // using software(functions) as node to build a software DFG
  //  val gsbf: ButterflyNode[Long] = ButterflyNode(nttAlgo.gsButterfly, "gsButterfly")
  //  val ctbf: ButterflyNode[Long] = ButterflyNode(nttAlgo.ctButterfly, "ctButterfly")
  //  val bitReverse: Seq[Long] => Seq[Long] = nttAlgo.bitReverse
  //
  //  val omega: Long = cfRing.getNthRoot(N)
  //  val inverseN: Long = cfRing.pow(N, -1)
  //  val coeffGen: Int => Long = (index: Int) => cfRing.pow(omega, index)
  //  val nttTestCase: Seq[Long] = (0 until N).map(_ => DSPRand.nextInt(p).toLong)
  //  val nttGolden: Seq[Long] = nttAlgo.NTT(nttTestCase)
  //
  //  implicit def converter(value: Long, width: BitCount): Long = value
  //
  //  "fft structure" should "be correct as NTT software" in {
  //    val nttDFG = ButterflyGen[Long, Long](ctbf, gsbf, N, DIF, inverse = false, coeffGen, -1 bits, 1).getGraph
  //    val nttImpl = new DFGImplSoft[Long](nttDFG)(0).implForwarding
  //    val nttYours = nttImpl(nttTestCase)
  //    logger.info(s"DFG    result:\n${nttYours.sorted.mkString(" ")}")
  //    logger.info(s"golden result:\n${nttGolden.sorted.mkString(" ")}")
  //    assert(nttYours.diff(nttGolden).isEmpty)
  //  }
  //
  //  it should "be correct as INTT software" in {
  //    val inttDFG = ButterflyGen[Long, Long](ctbf, gsbf, N, DIT, inverse = true, coeffGen, -1 bits, 1).getGraph
  //    val inttImpl = new DFGImplSoft[Long](inttDFG)(0).implForwarding
  //    val yours = inttImpl(bitReverse(nttGolden)).map(value => cfRing(value * inverseN))
  //    assert(yours.diff(nttTestCase).isEmpty)
  //  }
}

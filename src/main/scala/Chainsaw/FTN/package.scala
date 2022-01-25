package Chainsaw

import Chainsaw.algos.{MatlabRefs, Trellis}
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.reflect.ClassTag

package object FTN {

  var testSize = 1

  def runFTN(begin: Int, end: Int, bitAlloc: Boolean,
             useSyncData: Boolean = false, useGivenAlloc: Boolean = false, seedStart:Int = 0): Unit = {
    eng.eval("cd ~/FTN326")
    eng.eval(s"main([$begin:$end],$testSize,0,0, ${if (bitAlloc) "1" else "0"},${if (useSyncData) "1" else "0"},${if (useGivenAlloc) "1" else "0"}, $seedStart)")
  }

  // utils to get matlab data
  def loadFTN1d[T](name: String) = eng.load[Array[T]](s"~/FTN326/$name.mat", name)

  def loadFTN2d[T](name: String)(implicit tag: ClassTag[T]) =
    eng.load[Array[Array[T]]](s"~/FTN326/$name.mat", name).transpose.flatten

  case class ChannelInfo(bitAlloc: Array[Int], powAlloc: Array[Double], bitMask: Array[Int]) {
    val qamPositions = loadFTN1d[Double]("QAMPositions").map(_.toInt).map(_ - 1)
    val qamRemapPositions = loadFTN1d[Double]("QAMRemapPositions").map(_.toInt).map(_ - 1)

    val validCarrierNum = bitMask.sum
    val validFFTNum = (validCarrierNum + 1) * 2
  }

  case class FtnParams(start: Int, end: Int, doBitAlloc: Boolean,
                       useSyncData: Boolean = false,
                       useGivenAlloc: Boolean = false) {

    runFTN(start, end, doBitAlloc, useSyncData, useGivenAlloc)

    val channelInfo = {
      val bitAlloc = loadFTN1d[Double]("bitAlloc").map(_.toInt)
      val powAlloc = loadFTN1d[Double]("powAlloc").map(pow => pow * pow)
      val bitMask = loadFTN1d[Double]("bitMask").map(_.toInt)
      ChannelInfo(bitAlloc, powAlloc, bitMask)
    }

    val preambleScaled = loadFTN1d[Double]("preambleScaled").map(_.toInt)
    val fftSize = 512
    val cpLength = 20
    val frameLengthInRx = 16 // number of cycles that a frame would last in Rx

    val rangeLimit = eng.load[Double]("rangeLimit").toInt

    val txRaw: Seq[BigInt] = loadFTN1d[Double]("txRaw").map(_.toInt) // bits
      .grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2)) // vecSize = 128
    val txCoded: Seq[BigInt] = loadFTN1d[Double]("txCoded").map(_.toInt) // bits
      .grouped(256).toSeq.map(bit256 => BigInt(bit256.mkString(""), 2)) // vecSize = 256
    val txInterleaved: Seq[BigInt] = loadFTN1d[Double]("txInterleaved").map(_.toInt) // bits
      .grouped(256).toSeq.map(bit256 => BigInt(bit256.mkString(""), 2)) // vecSize = 256
    val txMapped: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("txMapped").map(_.toBComplex) // complex numbers
      .grouped(256).toSeq.map(_.toSeq) // vecSize = 256
    val txModulated: Seq[Seq[Double]] = loadFTN1d[Double]("txModulated").map(_ * 512.0) // real numbers
      .grouped(128).toSeq.map(_.toSeq) // vecSize = 128
    val txScaled: Seq[Seq[BigInt]] = loadFTN1d[Double]("txScaled").map(_.toInt).map(BigInt(_)).map(_ + 32)
      .grouped(128).toSeq.map(_.toSeq)
    val txPacked: Seq[Seq[BigInt]] = loadFTN1d[Double]("txFrame").map(_.toInt).map(BigInt(_)).map(_ + 32)
      .grouped(128).toSeq
      .map(_.padTo(128, BigInt(0)))
      .map(_.toSeq)

    // data for RxFront
    val rxModulated = loadFTN1d[Double]("rxScaled")
    val rxMapped = loadFTN1d[MComplex]("rxMapped").map(_.toBComplex)
    val rxEqualized = loadFTN1d[MComplex]("rxEqualized").map(_.toBComplex)

    val rxModulatedGolden: Seq[Seq[BigInt]] = { // Rx raw data
      val (preamble, data) = rxModulated.splitAt(1024)
      val padded = preamble.grouped(512).map(_.toSeq).toSeq ++ data.grouped(channelInfo.validFFTNum).map(_.toSeq.padTo(512, 0.0)).toSeq
      padded.flatten.grouped(128).toSeq
    }.map(_.map(value => BigInt(value.toInt)))
    val rxMappedGolden: Seq[Seq[BComplex]] = rxMapped.grouped(256).map(_.toSeq).toSeq // after fft in RxFront
    val rxEqualizedGolden: Seq[Seq[BComplex]] = rxEqualized.grouped(256).map(_.toSeq).toSeq // after equalization

    // data for RxLoop
    def boolSeq2BigInt(in: Seq[Int]) = BigInt(in.mkString(""), 2)

    val iterIn: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("iterIn").map(_.toBComplex).toSeq
      .grouped(256).toSeq // symbol
    val iter0: Seq[BigInt] = loadFTN1d[Double]("iter0").map(_.toInt).toSeq
      .grouped(1024).toSeq.map(boolSeq2BigInt) // after qamdemod
    val iter1: Seq[BigInt] = loadFTN1d[Double]("iter1").map(_.toInt).toSeq
      .grouped(1024).toSeq.map(boolSeq2BigInt) // after deInterleave
    val iter1ByGroup: Seq[BigInt] = loadFTN1d[Double]("iter1").map(_.toInt).toSeq
      .grouped(256).toSeq.map(boolSeq2BigInt) // after deInterleave
    val iter2: Seq[BigInt] = loadFTN1d[Double]("iter2").map(_.toInt).toSeq
      .grouped(512).toSeq.map(boolSeq2BigInt) // after vit
    val iter2ByGroup: Seq[BigInt] = loadFTN1d[Double]("iter2").map(_.toInt).toSeq
      .grouped(128).toSeq.map(boolSeq2BigInt) // after vit
    val iter3: Seq[BigInt] = loadFTN1d[Double]("iter3").map(_.toInt).toSeq
      .grouped(1024).toSeq.map(boolSeq2BigInt) // after conv
    val iter4: Seq[BigInt] = loadFTN1d[Double]("iter4").map(_.toInt).toSeq
      .grouped(1024).toSeq.map(boolSeq2BigInt) // after interleave
    val iter5 = loadFTN1d[MComplex]("iter5").map(_.toBComplex).toSeq
      .grouped(256).toSeq // after qammod
    val iter6 = loadFTN1d[Double]("iter6").toSeq.map(_ * 512.0)
      .grouped(512).toSeq // after ifft
    val iter7 = loadFTN1d[MComplex]("iter7").map(_.toBComplex).toSeq
      .grouped(256).toSeq // after fft
    val diff = loadFTN1d[MComplex]("diff").map(_.toBComplex).toSeq
      .grouped(256).toSeq // after diff

    // data for last iteration
    val finalDeInterleaved = loadFTN1d[Double]("rxFinalDeInterleaved").map(_.toInt).toSeq // after final qamdemod
      .grouped(1024).toSeq.map(boolSeq2BigInt)
    val finalDecoded: Seq[BigInt] = loadFTN1d[Double]("rxFinalDecoded").map(_.toInt).toSeq // after final vitdec
      .grouped(512).toSeq.map(boolSeq2BigInt)

    val txBitsAll = loadFTN1d[Double]("txBitsAll").map(_.toInt) // bits
      .grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2)) // vecSize = 128

    val txPackedAll = loadFTN1d[Double]("txPackedAll").map(_.toInt).map(BigInt(_)).map(_ + 32)
      .grouped((channelInfo.validFFTNum + cpLength) * 16 + (fftSize + cpLength) * 2).toSeq
      .flatMap(_.grouped(128).toSeq)
      .map(_.padTo(128, BigInt(0)))
      .map(_.toSeq)

    val rxFinalDecodedAll = loadFTN1d[Double]("rxFinalDecodedAll").map(_.toInt).toSeq // after final vitdec
      .grouped(512).toSeq.map(boolSeq2BigInt)

    // assertion
    assert(rxModulatedGolden.length == 18 * 4)
    Seq(iterIn, iter0, iter1, iter2, iter3, iter4, iter5, iter6, iter7).foreach(seq => assert(seq.length == 16))
  }

  // types
  val ADDAType = HardType(SInt(6 bits))
  val coeffType = HardType(SFix(1 exp, -15 exp))

  // types of symbols and fft/ifft are closely connected
  val symbolWidth = 16

  def getSymbolType(peak: Int) = HardType(SFix(peak exp, -(symbolWidth - 1 - peak) exp))

  // datapaths and datatypes for fft/ifft
  val fftDecomposition = Seq(4, 4, 4, 4, 2) // the way we construct a 512-point fft
  val ifftShifts = Seq(2, 2, 1, 0, 0)
  val fftShifts = Seq(2, 2, 1, 0, 0)
  val frontFftShifts = Seq(2, 2, 2, 0, 0) // this part has a wider dynamic range

  val Seq(symbolPeak, ifftPeak, fftPeak, addaPeak) =
    Seq(2, 2 + ifftShifts.sum, 2 + ifftShifts.sum + fftShifts.sum, 5)
  val Seq(symbolType, ifftType, fftType, addaFftType) =
    Seq(symbolPeak, ifftPeak, fftPeak, addaPeak).map(getSymbolType)
  val Seq(symbolComplexType, ifftComplexType, fftComplexType, addaFftComplexType) =
    Seq(symbolType, ifftType, fftType, addaFftType).map(toComplexType)

  // for equalizer computation
  val equalWidth = 18

  def getEqualType(peak: Int) = HardType(SFix(peak exp, -(equalWidth - 1 - peak) exp))

  val equalizerWidth = 256
  val smootherType = getEqualType(6) // tap = 16, 2 -> 6 for accumulation
  val smootherComplexType = toComplexType(smootherType)
  val smootherVecType = HardType(Vec(smootherType(), equalizerWidth))
  val smootherComplexVecType = HardType(Vec(ComplexNumber(smootherType), equalizerWidth))

  val equalizationType = getEqualType(6)
  val equalizationComplexType = toComplexType(equalizationType)
  val equalizationVecType = HardType(Vec(equalizationType(), equalizerWidth))
  val equalizationComplexVecType = HardType(Vec(ComplexNumber(equalizationType), equalizerWidth))

  // loading data for test
  val preambleSymbols = loadFTN1d[Double]("PreambleMappedSymbols").map(_.toInt)

  val matlabTrellis = MatlabRefs.poly2trellis(7, Array(171, 133))
  val trellis = Trellis.fromMatlab(matlabTrellis)

  // common transformations
  def bools2bits(in: Vec[Bool]) = in.reverse.asBits()

  def bits2bools(in: Bits) = Vec(in.asBools.reverse)

  // transformations for qammod/qamdemod
  def bitRemapAfterQamDemod(in: Bits)(implicit ftnParams: FtnParams) = {
    import ftnParams.channelInfo.{qamPositions, qamRemapPositions}
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamPositions.indexOf(i)
      if (index == -1) False else bools(qamRemapPositions(index))
    }.reverse.asBits()
  }

  def bitRemapBeforeQammod(in: Bits)(implicit ftnParams: FtnParams) = {
    import ftnParams.channelInfo.{qamPositions, qamRemapPositions}
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamRemapPositions.indexOf(i)
      if (index == -1) False else bools(qamPositions(index))
    }.reverse.asBits()
  }

  // mask out the unused channel
  def doBitMask(in: Vec[ComplexNumber])(implicit ftnParams: FtnParams) =
    Vec(in.zip(ftnParams.channelInfo.bitMask).map { case (data, mask) => if (mask == 1) data else in.head.getZero })

  // transformation for ifft/fft
  def truncComplex(in: Vec[ComplexNumber], dataType: HardType[SFix]) = Vec(in.map(_.truncated(dataType)))

  def shiftRight(in: Vec[ComplexNumber], shift: Int) = Vec(in.map(_ >> shift))

  // construct hermitian symmetric ifft input
  def ifftPre(in: Vec[ComplexNumber]) = Vec((0 until 512).map {
    case 0 => in.head.getZero
    case 256 => in.head.getZero
    case i => if (i < 256) in(i) else in(512 - i).conj
  })

  // take valid channels of ifft results
  def ifftPost(in: Vec[SFix])(implicit ftnParams: FtnParams) = {
    require(in.length == ftnParams.fftSize)
    Vec(in.zipWithIndex.map { case (fix, i) => if (i < ftnParams.channelInfo.validFFTNum) fix else fix.getZero })
  }

  // take half of the real-valued fft results
  def fftPost(in: Vec[ComplexNumber]) = Vec(in.take(in.length / 2))

  def prepareSyncData(start: Int, end: Int, dataPostFix: String, allocPostFix:String) = {
    printlnGreen(s"using data file ila${start}_${end}_$dataPostFix.csv")
    // get the params before using syncData
    prepareAlloc(allocPostFix)
    val ftnParams = FtnParams(start, end, doBitAlloc = true, useGivenAlloc = true) // get preamble with specified alloc
    val csvName = s"ila_${start}_${end}_$dataPostFix.csv"
    // get syncData
    val syncData = ILASync.sync(csvName, ftnParams)

    val preambleWithCP = ftnParams.fftSize + ftnParams.cpLength
    val validWithCP = ftnParams.channelInfo.validFFTNum + ftnParams.cpLength

    val syncDataForRx: Seq[Seq[BigInt]] = {
      val (preamble, data) = syncData._1.map(BigInt(_)).toSeq.splitAt(2 * preambleWithCP)
      val preambleWithOutCP = preamble.grouped(preambleWithCP).toSeq.flatMap(_.slice(ftnParams.cpLength / 2, ftnParams.cpLength / 2 + ftnParams.fftSize))
      val dataWithoutCp = data.grouped(validWithCP).toSeq.flatMap(_.slice(ftnParams.cpLength / 2, ftnParams.cpLength / 2 + ftnParams.channelInfo.validFFTNum))
      (preambleWithOutCP ++ dataWithoutCp).grouped(128).toSeq
    }

    eng.eval("cd ~/FTN326")
    eng.putVariable("syncData", syncDataForRx.flatten.map(_.toInt).toArray)
    eng.eval("save syncData syncData")

    (syncDataForRx, syncData._2)
  }

  def prepareAlloc(postFix: String): Unit = {
    eng.eval("cd ~/FTN326")
    eng.eval(s"load './allocs/bitAlloc$postFix.mat' bitAlloc$postFix \n givenBitAlloc = bitAlloc$postFix'; save givenBitAlloc givenBitAlloc;")
    eng.eval(s"load './allocs/powAlloc$postFix.mat' powAlloc$postFix \n givenPowAlloc = powAlloc$postFix'; save givenPowAlloc givenPowAlloc;")
  }

  // parameter for overall state machine
  def forEachParams(body: FtnParams => Unit): Unit = {
    val configs = Seq(
      //      (2, 256, false, ""),
      //      (3, 226, false, ""),
      (3, 226, true, "3_226_N15"),
      (3, 226, true, "3_226_N15_20KM"),
      (2, 256, true, "2_256_N15"),
      (2, 256, true, "2_256_N15_20KM")
    )
    configs.foreach { config =>
      if (config._4.nonEmpty) {
        prepareAlloc(config._4)
        body(FtnParams(config._1, config._2, config._3, useSyncData = false, useGivenAlloc = true))
      }
      else body(FtnParams(config._1, config._2, config._3))
    }
  }

  def reportBiterr(dutResult: Seq[BigInt])(implicit ftnParams: FtnParams) = {
    val txBits: Array[Int] = loadFTN1d[Double]("txRaw").map(_.toInt)
    val yourRx: Seq[Int] = dutResult.take(16).flatMap(_.toString(2).padToLeft(512, '0').map(_.asDigit))
    val biterr = yourRx.slice(32 * ftnParams.start - 1, 32 * ftnParams.end)
      .zip(txBits.slice(32 * ftnParams.start - 1, 32 * ftnParams.end))
      .count { case (rx, tx) => rx != tx } / 8192.0
    logger.info(s"bit err is $biterr")
    biterr
  }

}

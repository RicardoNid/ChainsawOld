package DSP

import breeze.linalg._
import breeze.math._
import breeze.numerics._

import scala.util.Random //  for digital signal processing

// bits, double and complex

object FTNParam {
  val on = true
  val CPLength = 20
  val PreambleNumber = 2
  val FFTSize = 512
  val OFDMSymbolNumber = 8

  val BitsPerSymbolQAM = 4
  val Seed = Array(10, 13, 21, 20, 8, 9, 15, 17, 19, 12, 11, 30, 25, 27, 26, 22, 14, 7, 23, 29) // seed for random message gen
  val PreambleBitsPerSymbolQAM = 4
  val PreambleSeed = 20 // seed for preamble gen

  val IterationNumber = 5 // iteration number for chow
  val DataCarrierPositions = (2 to 225).toArray // 224 in total, matlab position starts from 1, (3 to 226) in matlab
  val OFDMPositions = Array(0) ++ DataCarrierPositions ++ Array(FFTSize / 2) ++ DataCarrierPositions.map(FFTSize - _).sorted // 450 in total, 224 * 2 + 2

  val CodeRate = 0.5 // n = 2, k = 1
  val BitNumber = DataCarrierPositions.length * OFDMSymbolNumber * BitsPerSymbolQAM // 7168, 224 * 8 * 4
  val SToPcol = OFDMSymbolNumber / CodeRate // 16, 8 * 2

  val FrameNumber = 20

  val Premable = Array[Double]() // size = 1064, double, 1064 = PreambleNumber * (FFTSize + CPLength) = 2 * (512 + 20)

  val ConvConsLen = 7 // constraint length for convolutional encoder
  val ConvCodeGen = Array(121, 91) // octal [171,133]

  val InterDepth = 32 //

  val SmoothWidth = 20 // taps for moving average filter

  val tblen = 90

}

class FTNBetter {

  import FTNParam._

  var storedQAMSymbols: Array[Complex] = Array[Complex]()
  var storedS_HD: Array[Complex] = Array[Complex]()

  def RandomBits(): Array[Boolean] = {
    (0 until 20).map { i =>
      val r = new Random(Seed(i))
      (0 until BitNumber).map(_ => r.nextBoolean()).toArray
    }.toArray.flatten // size = 20 * 7168, bits
  }

  /** Hardware
   *
   * @param bitsOneFrame
   * @return
   */
  def ConvEncoder(bitsOneFrame: Array[Boolean]): Array[Boolean] = ???

  /** Hardware:
   *
   * @param convCodedMsg
   * @return
   */
  def Interleaver(convCodedMsg: Array[Boolean]): Array[Boolean] = ???

  def Qammod(interleavedMsg: Array[Boolean]): Array[Complex] = ???

  def Normalize(QAMSymbols: Array[Complex], sqrt10: Boolean): Array[Complex] = ???

  def IFFT(normalizedQAMSymbols: Array[Complex]): Array[Array[Double]] = ???

  def InsertCP(doubles: Array[Double]): Array[Double] = {
    val length = doubles.length
    doubles.slice(length - CPLength / 2, length) ++ doubles ++ doubles.slice(0, CPLength / 2)
  }

  def OFDMFrameGenerator(i: Int, bitsOneFrame: Array[Boolean]): Array[Double] = {
    val OFDMSymbols: Array[Double] = CreateOFDMSymbols(i, bitsOneFrame) // size = 7520, double. 7520 = SToPcol * (450 + CPLength)
    val OFDMSmallFrame = Premable ++ OFDMSymbols // size = 8584, double, 8584 = 1064 + 7520
    OFDMSmallFrame
  }

  // Hardware
  def OFDMTransmitter(bitsAllFrame: Array[Boolean]): Array[Double] = {
    val bitsOneFrame: Array[Array[Boolean]] = bitsAllFrame.grouped(BitNumber).toArray // size = 20 * 7168, bits
    val OFDMBigFrame: Array[Double] = (0 until FrameNumber) // size = 20 * 8584, double, 8584 = 7520 + 1064
      .map(i => OFDMFrameGenerator(i, bitsOneFrame(i)))
      .toArray.flatten
    OFDMBigFrame
  }

  def Channel(OFDMFrame: Array[Double]): Array[Double] = ???

  def ChannelEstimation(premable: Array[Double]): Array[Double] = ???

  def Smooth(H: Array[Double], SmoothWidth: Int): Array[Double] = ???

  def RecoverOFDMSymbolsWithPilot(message: Array[Double], smoothedH: Array[Double]): Array[Complex] = ???

  def Qamdemod(normalizedSymbols: Array[Complex]): Array[Boolean] = ???

  // Hardware, standard
  def DeInterleaver(QAMDemappedMsg: Array[Boolean]): Array[Boolean] = ???

  // Hardware, standard
  def VitDecoder(deinterleavedMsg: Array[Boolean]): Array[Boolean] = ???

  def RecoverOFDMSymbols(OFDMSymbolsWithCP: Array[Array[Double]]): Array[Complex] = ???

  def qfuncinv(d: Double): Double = ???

  def SNRLocation(diffSymbols: Array[Complex], SymbolsFromTrans: Array[Complex]): Array[Double] = ???

  def chow(SNR: Array[Double]): (Array[Int], Array[Double], Int) = ???

  def looping(frameId: Int, MsgForIteration: Array[Boolean], FDE: Array[Complex]) = {
    var MsgAfterIteration: Array[Boolean] = MsgForIteration //
    for (i <- 0 until IterationNumber) {
      // the same as transmitter
      val convCodedMsg: Array[Boolean] = ConvEncoder(MsgForIteration) // size = 14336, bits
      val interleavedMsg: Array[Boolean] = Interleaver(convCodedMsg) // size = 14336, bits
      val QAMSymbols: Array[Complex] = Qammod(interleavedMsg) // size = 3584, complex, 3584 = 14336 / BitsPerSymbolQAM
      val normalizedQAMSymbols: Array[Complex] = Normalize(QAMSymbols, false) // size = 3584, complex
      storedS_HD = normalizedQAMSymbols // size = 3584, complex
      val OFDMSymbols: Array[Array[Double]] = IFFT(normalizedQAMSymbols) // size = 7200 = 16 * 450, double, IFFT & Pick
      val OFDMSymbolsWithCP: Array[Array[Double]] = OFDMSymbols.map(InsertCP(_)) //  size = 7520 = 16 * 470, double, 470 = 450 + CPLength

      val recoveredSymbols: Array[Complex] = RecoverOFDMSymbols(OFDMSymbolsWithCP) // size = 3584, complex
      val normalizedRecoveredSymbols: Array[Complex] = Normalize(recoveredSymbols, true) // size = 3584, complex
      val ICI = normalizedRecoveredSymbols - storedS_HD // size = 3584, complex, elementwise -, supported by breeze, that's the amazing part of Scala implicit
      val diffSymbols = FDE - ICI // size = 3584, complex, elementwise -, supported by breeze, that's the amazing part of Scala implicit

      // the same as decoder
      val QAMDemappedMsg: Array[Boolean] = Qamdemod(diffSymbols) // size = 14336, bits, 14336 = 3584 * BitsPerSymbolQAM
      val deinterleavedMsg: Array[Boolean] = DeInterleaver(QAMDemappedMsg) // size = 14336, bits
      val convDecoedMsg: Array[Boolean] = VitDecoder(deinterleavedMsg) // size = 7168, bits, 7168 = 14336 * CodeRate

      val BitsFromTrans: Array[Boolean] = RandomBits().grouped(BitNumber).toArray.apply(i)
      MsgAfterIteration = convDecoedMsg.slice(tblen, convDecoedMsg.length) ++
        BitsFromTrans.slice(BitsFromTrans.length - tblen, BitsFromTrans.length)

      if (i == IterationNumber && frameId == FrameNumber) {
        val SymbolsFromTrans = storedQAMSymbols.map(_ * sqrt(10)) // size = 3584, complex
        val SNR: Array[Double] = SNRLocation(diffSymbols, SymbolsFromTrans) // size = 224, double
        val (bitsAlloc: Array[Int], powerAlloc: Array[Double], totalBits: Int) = chow(SNR) // size = 224, 224, 1, int, double, int
      }
    }
  }

  def OFDMFrameDecoder(i: Int, dataOneFrame: Array[Double]): Array[Boolean] = {
    val premable: Array[Double] = dataOneFrame.slice(0, PreambleNumber * (FFTSize + CPLength)) // size = 1064, double
    val message = dataOneFrame.slice(PreambleNumber * (FFTSize + CPLength), dataOneFrame.length) // size = 7520, double

    val H: Array[Double] = ChannelEstimation(premable) // size = 255, double
    val smoothedH: Array[Double] = Smooth(H, SmoothWidth) // size = 255, double
    val recoveredSymbols: Array[Complex] = RecoverOFDMSymbolsWithPilot(message, smoothedH) // size = 3584, complex, 3584 = 16 * 224
    val normalizedRecoveredSymbols: Array[Complex] = Normalize(recoveredSymbols, true) // size = 3584, complex
    val FDE: Array[Complex] = normalizedRecoveredSymbols // fork for looping
    val QAMDemappedMsg: Array[Boolean] = Qamdemod(normalizedRecoveredSymbols) // size = 14336, bits, 14336 = 3584 * BitsPerSymbolQAM
    val deinterleavedMsg: Array[Boolean] = DeInterleaver(QAMDemappedMsg) // size = 14336, bits
    val convDecoedMsg: Array[Boolean] = VitDecoder(deinterleavedMsg) // size = 7168, bits, 7168 = 14336 * CodeRate

    val BitsFromTrans: Array[Boolean] = RandomBits().grouped(BitNumber).toArray.apply(i)
    val MsgForIteration: Array[Boolean] = // size = 7168, bits
      convDecoedMsg.slice(tblen, convDecoedMsg.length) ++
        BitsFromTrans.slice(BitsFromTrans.length - tblen, BitsFromTrans.length)

    looping(i, MsgForIteration, FDE)
    convDecoedMsg // size = 7168, bits
  }

  def CreateOFDMSymbols(i: Int, bitsOneFrame: Array[Boolean]): Array[Double] = {
    val convCodedMsg: Array[Boolean] = ConvEncoder(bitsOneFrame) // size = 14336, bits
    val interleavedMsg: Array[Boolean] = Interleaver(convCodedMsg) // size = 14336, bits
    val QAMSymbols: Array[Complex] = Qammod(interleavedMsg) // size = 3584, complex, 3584 = 14336 / BitsPerSymbolQAM
    val normalizedQAMSymbols: Array[Complex] = Normalize(QAMSymbols, false) // size = 3584, complex
    storedQAMSymbols = normalizedQAMSymbols // size = 3584, complex, store it
    val OFDMSymbols: Array[Array[Double]] = IFFT(normalizedQAMSymbols) // size = 7200 = 16 * 450, double, IFFT & Pick
    val OFDMSymbolsWithCP: Array[Array[Double]] = OFDMSymbols.map(InsertCP(_)) //  size = 7520 = 16 * 470, double, 470 = 450 + CPLength
    OFDMSymbolsWithCP.flatten
  }

  // Hardware
  def OFDMReceiver(OFDMFrameRec: Array[Double]): Array[Boolean] = {
    val dataOneFrame: Array[Array[Double]] = OFDMFrameRec.grouped(BitNumber).toArray // size = 20 * 8584, double
    val OFDMFramesDecoded: Array[Boolean] = (0 until FrameNumber) // size = 20 * 7168 bits
      .map(i => OFDMFrameDecoder(i, dataOneFrame(i)))
      .toArray.flatten
    OFDMFramesDecoded
  }

  // MATLAB
  def biterr(bitsAllFrame: Array[Boolean], debitsAllFrame: Array[Boolean]): Int = {
    require(bitsAllFrame.length == debitsAllFrame.length)
    bitsAllFrame.zip(debitsAllFrame).filter { case (bool, bool1) => bool != bool1 }.size
  }

  def FTN = {
    val bitsAllFrame: Array[Boolean] = RandomBits() // size = 143360, bits, 143360 = FrameNumber * BitNumber = 20 * 7168
    printlnWhenDebug("input gen done")
    val OFDMFrame: Array[Double] = OFDMTransmitter(bitsAllFrame) // size = 171680, double, 171680 = FrameNumber * TODO
    printlnWhenDebug("transmitting done")
    val OFDMFrameRec: Array[Double] = Channel(OFDMFrame) // size = 171680, double, 171680 = FrameNumber * 
    val debitsAllFrame: Array[Boolean] = OFDMReceiver(OFDMFrameRec) // size = 143360, bits, 143360 = FrameNumber * BitNumber = 20 * 7168
    val nErrors: Int = biterr(bitsAllFrame, debitsAllFrame)
    val BER = nErrors.toDouble / bitsAllFrame.length
    BER
  }

}

object FTNBetter {

  import FTNParam._

  def main(args: Array[String]): Unit = {
    println(DataCarrierPositions.length)
    println(DataCarrierPositions.mkString(" "))
    println(OFDMPositions.length)
    println(OFDMPositions.mkString(" "))

    val ber = new FTNBetter().FTN
  }
}
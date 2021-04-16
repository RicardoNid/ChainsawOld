//package DSP
//
//import breeze.linalg.Matrix.{castOps, zeros}
//import breeze.linalg._
//import breeze.numerics._
//import breeze.linalg._
//import breeze.math._
//import breeze.signal._
//
//import scala.util.Random //  for digital signal processing
//
//case class OFDMParameters(
//                           on: Int = 1, // 1 for runing, 0 for bit allocation
//                           CPLength: Int = 20, // CP = cyclic prefix
//                           PreambleNumber: Int = 2, // TODO: ?
//
//                           FFTSize: Int = 512,
//                           OFDMSymbolNumber: Int = 8,
//                           BitsPerSymbolQAM: Int = 4,
//                           PreambleBitsPerSymbolQAM: Int = 4,
//
//                           iteration: Int = 5, // TODO: ?
//
//                           DataCarrierPositions: Array[Int] = (3 to 226).toArray,
//
//                           Seed: Array[Int] = Array(10, 13, 21, 20, 8, 9, 15, 17, 19, 12, 11, 30, 25, 27, 26, 22, 14, 7, 23, 29), // seed for different frams(for random gen)
//                           PreambleSeed: Int = 20,
//                           // k = 1, n = 2, codeRate = 1/2
//                           codeRate: Double = 1.0 / 2.0) {
//  val OFDMPositions = (Array(1) ++ DataCarrierPositions ++ Array(FFTSize / 2 + 1) ++ DataCarrierPositions.map(FFTSize + 2 - _)).sorted
//  val PreambleCarrierPositions = (-FFTSize / 2 + 1 to -1).toArray.map(_ + FFTSize / 2 + 1)
//  val bitNumber = OFDMSymbolNumber * DataCarrierPositions.length * BitsPerSymbolQAM
//  val SToPcol = bitNumber * (1 / codeRate).toInt / BitsPerSymbolQAM / DataCarrierPositions.length // TODO: ?
//}
//
//case class Trellis(numInputSymbols: Int, numOutputSymbol: Int, numStates: Int,
//                   nextStates: Array[Array[Int]], outputs: Array[Array[Int]]) {
//}
//
//case class QammodObject(constellation: Array[Complex], M: Int, symbolMapping: Array[Int])
//case class QamdemodObjet(constellation: Array[Complex], M: Int, symbolMapping: Array[Int], DecisionType: Int)
//
//class FTN {
//
//}
//
//object FTN {
//
//  val param = OFDMParameters()
//
//  def ChannelEstimationByPreamble(preamble: Array[Double]): Array[Complex] = ???
//
//  def smooth(H: Array[Complex], tap: Int): Array[Complex] = ???
//
//  def RecoverOFDMSymbolsWithPilot(message: Array[Double], smoothedH: Array[Complex]):Array[Complex] = ???
//
//  def OFDMFrameReceiver(frame: Array[Double], iteration: Int) = {
//    val preambleLength = (param.FFTSize + param.CPLength) * param.PreambleNumber
//    val preamble = frame.slice(0, preambleLength)
//    // channel estimation
//    val H = ChannelEstimationByPreamble(preamble) // 255 * 1 complex
//    val tap = 20
//    val smoothedH = smooth(H, tap) // smoothed by 20-tap moving average
//    val message = frame.slice(preambleLength, frame.length) // 7520 * 1 double
//    // recover message
//    val recoveredMessage = RecoverOFDMSymbolsWithPilot(message, smoothedH) // 3584 * 1 complex
//    val rms = recoveredMessage.map(_.abs).sum / recoveredMessage.length
//    val normalizedRecoveredMessage = recoveredMessage.map(_ / rms * sqrt(10))
//
//    val constlen = 7
//    val codegen = Array(Array(171,133))
//    val tblen = 90
//    val trellis = poly2trellis(constlen, codegen)
//
//    val demodulatedMessage = demodulate(normalizedRecoveredMessage) // hard decision
//    val deinterleavedMessage = deinterleve(demodulatedMessage)
//
//
//
//  }
//
//  def poly2trellis(constlen: Int, codegen: Array[Array[Int]]): Trellis = {
//    Trellis(
//      numInputSymbols = 1 << codegen.length,
//      numOutputSymbol = 1 << codegen(0).length,
//      numStates = 128, // TODO
//      nextStates = Array[Array[Int]](), // TODO
//      outputs = Array[Array[Int]]()
//    )
//  }
//
//
//  def main(args: Array[String]): Unit = {
//    val BER_SNR = DenseVector[Double]() // BER = bit error rate
//    val BER_MC_Sim_total = DenseVector[Double]()
//
//    def OFDMFrameGenerator(iteration: Int): Array[Double] = {
//      createOFDMSymbols(iteration) ++ createOFDMPreamble
//    }
//
//    def GrayQAMCoder(bits: Array[Boolean], BitsPerSymbol: Int): Array[Complex] = {
//      Array[Complex]()
//    }
//
//    def createOFDMSymbols(iteration: Int): Array[Double] = {
//
//      param.on match {
//        case 0 => {
//          // generate data randomly
//          val r = new Random(param.Seed(iteration))
//          val bits = (0 until param.bitNumber).map(_ => r.nextInt(2)).toArray // 7168 bits
//          // convolutional encoding
//          val bitsPerFrame = bits
//          val constlen = 7
//          val codegen = Array(Array(171, 133))
//          val trellis = poly2trellis(constlen, codegen)
//          val codedMsg = convenc(bits, trellis) // 14336 bits
//          // interleaving
//          val depth = 32
//          val codeMsg = interleave(codedMsg, depth) // 14336 bits, after interleaving
//          // TODO: transpose it as a column/row vector
//          // mapping
//          val M = 1 << param.BitsPerSymbolQAM
//          val modObj = QammodObject(
//            constellation = Array[Complex](),
//            M = M,
//            symbolMapping = Array[Int](),
//          ) // TODO
//          val QAMSymbols = modulate(modObj, codeMsg) // 4 bits -> 1 symbol, 3584 complex
//          val rootMeanSquare = rms(QAMSymbols)
//          val normalizedQAMSymbols = QAMSymbols.map(_ / rootMeanSquare)
//          val groupedQAMSymbols = normalizedQAMSymbols.grouped(param.SToPcol).toArray // 224 group of 16 symbols
//          // fill symbols and its conjugate into the ifft block
//          val ifftBlock = Array.tabulate(param.FFTSize, param.SToPcol)(Complex(_, _)) // 512 * 16 complex
//          param.DataCarrierPositions.foreach(i => ifftBlock(i) = groupedQAMSymbols(i))
//          param.DataCarrierPositions.foreach(i => ifftBlock(param.FFTSize + 2 - i) = ifftBlock(i).map(_.conjugate))
//          // ifft
//          val OFDMSymbols = ifftBlock.map(sequence => iFourierTr.dvComplexIFFT(DenseVector(sequence))) // 512 * 16 double(?), 16 sequences of ifft result
//          // slice the CP part
//          val OFDMSymbols1 = OFDMSymbols.slice(0, param.OFDMPositions.length)
//          //          val OFDMSymbols2 = OFDMSymbols1.slice() TODO
//          OFDMSymbols1.map(_.map(complex => complex.real + complex.imag).toArray).flatten // TODO
//        }
//        case 1 => { // TODO
//          //          load('bitAllocSort.mat');
//          //          load('BitAllocSum.mat');
//          //          load('power_alloc.mat');
//          val rmsAlloc = Array[Double]()
//          val bits = Array[Boolean]()
//          rmsAlloc
//        }
//      }
//    }
//
//    def createOFDMPreamble: Array[Double] = {
//      val bitNumber = param.PreambleCarrierPositions.length * param.PreambleBitsPerSymbolQAM
//      val preambleBits = randomBits(bitNumber, param.PreambleSeed)
//      val preambleQAMSymbols = GrayQAMCoder(preambleBits, param.BitsPerSymbolQAM)
//      val normalizedPreambleQAMSymbols = rms(preambleQAMSymbols)
//
//      val ifftBlocks = zeros[Complex](param.FFTSize, 1)
//
//      Array[Double]()
//    }
//
//    def OFDMBigFrameGenerator: Array[Double] = {
//      val OFDMBigFrame = (0 until 20).map(i => createOFDMPreamble ++ OFDMFrameGenerator(i)).flatten
//      // TODO: transpose it as a column/row vector
//      OFDMBigFrame.toArray
//    }
//
//    def randomBits(length: Int, seed: Int) = {
//      val r = new Random(seed)
//      (0 until length).map(_ => r.nextBoolean()).toArray
//    }
//
//    def rms(array: Array[Complex]) = array.map(_.abs)
//      .sum / array.length
//
//
//    // Gray coding
//    def modulate(qammod: QammodObject, msg: Array[Int]) = {
//      msg.grouped(qammod.M).map(_ => qammod.constellation(0)).toArray // TODO
//    }
//
//    def interleave(msg: Array[Int], depth: Int) = {
//      val groups = msg.zipWithIndex
//        .groupBy { case (bit, i) => i % depth }
//      (0 until depth).map(i => groups.get(i).get)
//        .map(_.map(_._2))
//        .flatten.toArray
//    }
//
//    def convenc(bits: Array[Int], trellis: Trellis): Array[Int] = {
//      bits.map(_ => 0) // TODO
//    }
//
//
//  }
//}

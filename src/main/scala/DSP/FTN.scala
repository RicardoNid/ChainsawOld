package DSP

import breeze.linalg._ //  for digital signal processing

case class OFDMParameters(
                           on: Int = 1, // 1 for runing, 0 for bit allocation
                           CPLength: Int = 20, // CP = cyclic prefix
                           PreambleNumber: Int = 2, // TODO: ?

                           FFTSize: Int = 512,
                           OFDMSymbolNumber: Int = 8,
                           BitsPerSymbolQAM: Int = 4,
                           PreambleBitsPerSymbolQAM: Int = 4,

                           iteration: Int = 5, // TODO: ?

                           DataCarrierPositions: Array[Int] = (3 to 226).toArray,

                           Seed: Array[Int] = Array(10, 13, 21, 20, 8, 9, 15, 17, 19, 12, 11, 30, 25, 27, 26, 22, 14, 7, 23, 29), // seed for different frams(for random gen)
                           PreambleSeed: Int = 20,
                           // k = 1, n = 2, codeRate = 1/2
                           codeRate: Double = 1.0 / 2.0) {
  val OFDMPositions = (Array(1) ++ DataCarrierPositions ++ Array(FFTSize / 2 + 1) ++ DataCarrierPositions.map(FFTSize + 2 - _)).sorted
  val PreambleCarrierPositions = (-FFTSize / 2 + 1 to -1).toArray.map(_ + FFTSize / 2 + 1)
  val bitNumber = OFDMSymbolNumber * DataCarrierPositions.length * BitsPerSymbolQAM
  val SToPcol = bitNumber * (1 / codeRate).toInt / BitsPerSymbolQAM / DataCarrierPositions.length // TODO: ?
}

class FTN {

}

object FTN {

  val param = OFDMParameters()

  def main(args: Array[String]): Unit = {
    val BER_SNR = DenseVector[Double]() // BER = bit error rate
    val BER_MC_Sim_total = DenseVector[Double]()

    def OFDMFrameGenerator: Array[Double] = {
      param.on match {
        case 0 => {
          val bits =

        }
      }
    }


    def OFDMBigFrameGenerator: Array[Double] = {
      val OFDMBigFrame = (0 until 20).map(_ => OFDMFrameGenerator).flatten
      // reshape it as a column vector
      OFDMBigFrame.toArray
    }

    def CreateOFDMSymbols: Array[Double] = {
      //      load('bitAllocSort.mat');
      //      load('BitAllocSum.mat');
      //      load('power_alloc.mat');

    }

    def CreateOFDMPreamble: Array[Double]

  }
}

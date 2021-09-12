import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._

package object FTN {

  val FTNMatlabWorkSpace = "/home/ltr/FTN326"

  // algo parameters
  case class FTNPARAMS(params: MStruct) {

    // params on modulation(FFT/IFFT)
    val FFTSize = params.get("FFTSize").asInstanceOf[Double].toInt
    val CPLength = params.get("CPLength").asInstanceOf[Double].toInt

    val CarrierNum = FFTSize / 2
    val DataCarrierPositions = params.get("DataCarrierPositions").asInstanceOf[Array[Double]].map(_.toInt)

    val QAMRms = params.get("QAMRms").asInstanceOf[Array[Double]]
    val SpecialQAM8Symbols = params.get("SpecialQAM8Symbols").asInstanceOf[Array[MComplex]]

    val QAMPositions = params.get("QAMPositions").asInstanceOf[Array[Double]].map(_.toInt)

    val InterleaveRow = params.get("InterleaveRow").asInstanceOf[Double].toInt
    val InterleaveCol = params.get("InterleaveCol").asInstanceOf[Double].toInt
    val BitsPerFramePadded = params.get("BitsPerFramePadded").asInstanceOf[Double].toInt
    val BitsPerSymbol = params.get("BitsPerSymbol").asInstanceOf[Double].toInt
    val SymbolsPerChannel = params.get("SymbolsPerChannel").asInstanceOf[Double].toInt

    eng.eval(s"cd $FTNMatlabWorkSpace; \n" +
      "load bitAlloc bitAlloc; \n" +
      "load powAlloc powAlloc ; \n"
    )
    val bitAlloc = eng.getVariable("bitAlloc").asInstanceOf[Array[Double]].map(_.toInt)
    val powAlloc = eng.getVariable("powAlloc").asInstanceOf[Array[Double]]
    val bitMask = (0 until CarrierNum).map(i => DataCarrierPositions.contains(i + 1)).toArray

    printlnGreen(bitMask.mkString(" "))

  }

  lazy val params: FTNPARAMS = {
    eng.eval(s"cd $FTNMatlabWorkSpace; \n" +
      "channel = 3:226; \n" +
      "InitPARAMS(2, channel); \n" +
      "load PARAMS PARAMS; \n" +
      "PARAMS")
    FTNPARAMS(eng.getVariable[MStruct]("PARAMS"))
  }

  val convencConfig = ConvencConfig(7, Array(171, 133))

  // hardware parameters
  // parameters on dataType
  val resolution = -7
  val peak = 4
  val wordWidth = 1 + peak - resolution

  def toSFix(value: Double) = SF(value, peak exp, resolution exp)
  val coeffType = HardType(SFix(1 exp, -10 exp))

  val qamFixedType = HardType(SFix(1 exp, -10 exp))
  val qamSymbolType = HardType(ComplexNumber(qamFixedType))

  val ifftFixedType = HardType(SFix(7 exp, -8 exp))
  val ifftSymbolType = HardType(ComplexNumber(ifftFixedType))

  val coeffFixedType = HardType(SFix(1 exp, -11 exp))

  //  val complexType = HardType(ComplexNumber(peak, resolution))
  val complexType = qamSymbolType

  val pFNonIter = 128 // pF for non-iterative part, denoted as the pF of initially issued bits(from BitGen)
  val pFIter = 512
}

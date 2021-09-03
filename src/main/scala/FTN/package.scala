import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._

package object FTN {

  // algo parameters
  case class FTNPARAMS(params: MStruct) {
    // params on modulation(FFT/IFFT)
    val FFTSize = params.get("FFTSize").asInstanceOf[Double].toInt
    val CPLength = params.get("CPLength").asInstanceOf[Double].toInt
    val DataCarrierPositions = params.get("DataCarrierPositions").asInstanceOf[Array[Double]]
    val InterleaveRow = params.get("InterleaveRow").asInstanceOf[Double].toInt
    val InterleaveCol = params.get("InterleaveCol").asInstanceOf[Double].toInt
    val BitsPerFramePadded = params.get("BitsPerFramePadded").asInstanceOf[Double].toInt
  }

  lazy val params: FTNPARAMS = {
    eng.eval("cd /home/ltr/IdeaProjects/Chainsaw/matlabWorkspace/FTN326; \n" +
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

  val fixedType = HardType(SFix(peak exp, resolution exp))
  val coeffType = HardType(SFix(1 exp, -10 exp))
  val complexType = HardType(ComplexNumber(peak, resolution))

  val pFNonIter = 128 // pF for non-iterative part, denoted as the pF of initially issued bits(from BitGen)
  val pFIter = 512
}

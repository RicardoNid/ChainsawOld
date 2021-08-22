//package FTN
//
//import spinal.core._
//
//class Tx(pF: Int) extends Component {
//  val dataIn = Bits(pF bits)
//
//  val convencConfig = ConvencConfig(7, Array(171, 133))
//  val convencFTN = ConvencFTN(convencConfig, pF)
//
//  convencFTN.dataIn := dataIn
//  val coded = convencFTN.dataOut
//
//  val interleaverFTN = InterleaverFTN(32, 128, pF * 2)
//  interleaverFTN.dataIn := coded
//  val interleaved = interleaverFTN.dataOut
//
//  val bitAlloc = Array.fill(pF)(4)
//  val qammodFTN = QammodFTN(bitAlloc, pF)
//  qammodFTN.dataIn := interleaved
//  val dataOut = qammodFTN.dataOut
//}

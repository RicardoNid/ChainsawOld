package Chainsaw.comm.channelCoding

import Chainsaw.{GenRTL, VivadoSynth, VivadoSynthForTiming, algos, logger}
import Chainsaw.dspTest._
import breeze.linalg.{DenseMatrix, DenseVector, convert}
import spinal.core._
import spinal.lib._

case class ConvencFTNRx(copies:Int = 512)
  extends Component with DSPTestable[Vec[UInt], Vec[UInt]] {

  val string171 = "1111001"
  val string133 = "1011011"
  val convenc: Seq[Bool] => Bits = (bools: Seq[Bool]) =>
    bools.zip(string171).filter(_._2 == '1').map(_._1).xorR ##
      bools.zip(string133).filter(_._2 == '1').map(_._1).xorR

  case class Convenc802() extends Component {
    val reset = in Bool()
    val dataIn = in UInt(1 bits)
    val dataOut = out UInt(2 bits)

    val stateReg = RegInit(B"000000")
    dataOut := RegNext(convenc((stateReg ## dataIn).asBools).asUInt)
    stateReg := Mux(reset, stateReg.getZero, stateReg(4 downto 0) ## dataIn)
  }

  val convencs = Seq.fill(copies)(Convenc802())

  val dataIn = slave Stream Vec(UInt(1 bits), copies)
  val dataOut = master Stream Vec(UInt(2 bits), copies)

  val counter = Counter(128)
  when(dataIn.fire || counter =/= U(0))(counter.increment())

  val reset = counter.valueNext === U(0)
  dataIn.payload.zip(dataOut.payload).zip(convencs).foreach{ case ((in, out), conv) =>
    conv.reset := reset
    conv.dataIn := in
    out := conv.dataOut
  }

  dataIn.ready := True
  dataOut.valid := RegNext(dataIn.valid, init = False)

  val latency = 1
}

object ConvencFTNRx extends App {

  val testMatrix = convert(DenseMatrix.rand[Double](512, 128) + 0.5, Int)

  val testCases = (0 until 128).map(i => testMatrix(::, i)).map(dv => BigInt(dv.toArray.mkString(""), 2))

  val convData: Seq[DenseVector[Int]] = (0 until 512).map(i => testMatrix(i, ::).t)
  val trellis = algos.MatlabRefs.poly2trellis(7, Array(171, 133))
  val codedData = convData.map(algos.MatlabRefs.convenc(_, trellis))

  val goldens = (0 until 128).map(i => BigInt(codedData.map(dv => dv(2 * i).toString + dv(2 * i + 1).toString).mkString(""), 2))

  doFlowPeekPokeTest("testConvenc512", ConvencFTNRx(), testCases, goldens)

//  VivadoSynthForTiming(Convenc512FTN())

}

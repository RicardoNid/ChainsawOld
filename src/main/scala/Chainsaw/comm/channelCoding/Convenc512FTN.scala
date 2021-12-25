package Chainsaw.comm.channelCoding

import Chainsaw.{GenRTL, VivadoSynth, VivadoSynthForTiming, algos, logger}
import Chainsaw.dspTest._
import breeze.linalg.{DenseMatrix, DenseVector, convert}
import spinal.core._
import spinal.lib._

case class Convenc512FTN() extends Component with DSPTestable[Bits, Bits] {

  val dataIn = slave Stream Bits(512 bits)
  val dataOut = master Stream Bits(1024 bits)

  val stateRegs = Vec(RegInit(B"000000"), 512)
  val counter = Counter(128)
  when(dataIn.fire || counter =/= U(0))(counter.increment())

  val string171 = "1111001"
  val string133 = "1011011"
  val convenc = (bools: Seq[Bool]) =>
    bools.zip(string171).filter(_._2 == '1').map(_._1).xorR ##
      bools.zip(string133).filter(_._2 == '1').map(_._1).xorR

  (0 until 512).foreach { i =>
    val state = stateRegs(i)
    val in = dataIn.payload(i)
    val out = dataOut.payload(i * 2 + 1 downto i * 2)
    state := Mux(counter.willOverflow, state.getZero, state(4 downto 0) ## in)
    out := RegNext(convenc((state ## in).asBools))
  }

  dataIn.ready := True
  dataOut.valid := RegNext(dataIn.valid, init = False)

  val latency = 1
}

object Convenc512FTN extends App {

//  val testMatrix = convert(DenseMatrix.rand[Double](512, 128) + 0.5, Int)
//
//  val testCases = (0 until 128).map(i => testMatrix(::, i)).map(dv => BigInt(dv.toArray.mkString(""), 2))
//
//  val convData: Seq[DenseVector[Int]] = (0 until 512).map(i => testMatrix(i, ::).t)
//  val trellis = algos.MatlabRefs.poly2trellis(7, Array(171, 133))
//  val codedData = convData.map(algos.MatlabRefs.convenc(_, trellis))
//
//  val goldens = (0 until 128).map(i => BigInt(codedData.map(dv => dv(2 * i).toString + dv(2 * i + 1).toString).mkString(""), 2))
//
//  doFlowPeekPokeTest("testConvenc512", Convenc512FTN(), testCases, goldens)

  VivadoSynthForTiming(Convenc512FTN())

}

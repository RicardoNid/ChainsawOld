package Chainsaw.comm.channelCoding

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class Convenc128FTN()
  extends Component with DSPTestable[Bits, Bits] {

  val dataIn = slave Stream Bits(128 bits)
  val dataOut = master Stream Bits(256 bits)

  // as parallelism >= block size & termination mode is termination, states are not needed

  val string171 = "1111001"
  val string133 = "1011011"
  val convenc = (bools: Seq[Bool]) =>
    bools.zip(string171).filter(_._2 == '1').map(_._1).xorR ##
      bools.zip(string133).filter(_._2 == '1').map(_._1).xorR

  val ret = (B"000000" ## dataIn.payload).asBools
    .sliding(7).toSeq
    .map(convenc).asBits()

  dataOut.payload := RegNext(ret)
  dataIn.ready := True
  dataOut.valid := RegNext(dataIn.valid, init = False)

  val latency = 1
}

object Convenc128FTN extends App {

//  val trellis = algos.MatlabRefs.poly2trellis(7, Array(171, 133))
//  val testCases = (0 until 10).map(_ => ChainsawRand.nextBigInt(128))
//  val goldens = testCases.map { data =>
//    val dataDV = new DenseVector(data.toString(2).padToLeft(128, '0').map(_.asDigit).toArray)
//    val retDv = algos.MatlabRefs.convenc(dataDV, trellis)
//    BigInt(retDv.toArray.mkString(""), 2)
//  }
//
//  doFlowPeekPokeTest("testConvenc128", Convenc128FTN(), testCases, goldens)

  VivadoSynthForTiming(Convenc128FTN())
}
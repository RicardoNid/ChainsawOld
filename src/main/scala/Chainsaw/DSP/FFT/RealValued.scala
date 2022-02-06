package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Algos.{fold, bitReverse}

import scala.collection.mutable.Stack


/** radix-2, real-valued FFT
 *
 */
case class R2RVFFT(N: Int = 256, dataType: HardType[SFix], coeffType: HardType[SFix])
                  (implicit complexMultConfig: ComplexMultConfig = ComplexMultConfig())
  extends Component {

  val complexType = HardType(ComplexNumber(dataType))
  val dataIn = slave Flow Vec(dataType, N)
  val dataOut = master Flow Vec(complexType, N / 2)

  def butterflyReal(data: Seq[SFix]): Seq[SFix] = fold(data).map { case (d, d1) => d + d1 } ++ fold(data).map { case (d, d1) => d - d1 }

  def butterflyComplex(data: Seq[ComplexNumber]): Seq[ComplexNumber] = fold(data).map { case (d, d1) => d + d1 } ++ fold(data).map { case (d, d1) => d - d1 }

  def swap(data: Seq[SFix]) = fold(data).map { case (d, d1) => ComplexNumber(d, -d1) }

  val stageMax = log2Up(N)

  implicit val multLatency = 2

  // Fig1, CFFT
  def fig1(data: Seq[ComplexNumber], stage: Int): Seq[ComplexNumber] = {
    if (stage == stageMax) butterflyComplex(data)
    else {
      val half = data.length / 2
      val (butterflied0, butterflied1) = RegNext(Vec(butterflyComplex(data))).splitAt(half)
      val indices = (0 until half).map(_ * 1 << (stage - 1))
      val multiplied = butterflied1.zip(indices).map { case (complex, i) => multiplyWNnk(complex, coeffType, i, N) }
      val delayed = Delay(Vec(butterflied0), multLatency)
      fig1(delayed, stage + 1) ++ fig1(multiplied, stage + 1)
    }
  }

  // Fig2, afterReduction
  def fig2(data: Seq[SFix], stage: Int): Seq[ComplexNumber] = {
    //      println(s"fig2 stage$stage")
    if (stage == stageMax) butterflyReal(data).map(ComplexNumber(_, dataType().getZero))
    else {
      val half = data.length / 2
      val (butterflied0, butterflied1) = RegNext(Vec(butterflyReal(data))).splitAt(half)
      val indices = (0 until half).map(_ * 1 << (stage - 1))
      val multiplied = swap(butterflied1).zip(indices).map { case (complex, i) => multiplyWNnk(complex, coeffType, i, N) }
      val delayed = Delay(Vec(butterflied0), multLatency)
      if (multiplied.length > 1) fig2(delayed, stage + 1) ++ fig1(multiplied, stage + 2).padTo(half, complexType().getZero)
      else fig2(delayed, stage + 1) ++ multiplied.padTo(half, complexType().getZero)
    }
  }

  val reverseIndices = (0 until N).map(bitReverse(N, _))
  val droppedIndices = (0 until N).filter { i =>
    val up = 1 << log2Up(i + 1)
    val level = up / 4
    level != 0 && (i / level) % 4 == 3
  }.map(bitReverse(N, _))

  val bitReversRet = fig2(dataIn.payload, 1)
  val orderedRet = reverseIndices.map(bitReversRet.apply(_))
  val ret = (0 until (N / 2)).map(i => if (droppedIndices.contains(i)) orderedRet(N - i).conj else orderedRet(i))
  dataOut.payload := Vec(ret)

  def latency = LatencyAnalysis(dataIn.payload.head.raw, dataOut.payload.head.real.raw)

  dataOut.valid := Delay(dataIn.valid, latency, init = False)

  // for sim
  dataOut.payload.simPublic()
}

object R2RVFFT {
  def main(args: Array[String]): Unit = {
    GenRTL(R2RVFFT(64, HardType(SFix(7 exp, -8 exp)), HardType(SFix(1 exp, -11 exp))))
  }
}


/** radix-2, hermitian symmetric IFFT
 *
 */
case class R2HSIFFT(N: Int = 256, dataType: HardType[SFix], coeffType: HardType[SFix])
                   (implicit complexMultConfig: ComplexMultConfig = ComplexMultConfig())
  extends Component {

  val complexType = HardType(ComplexNumber(dataType))
  val dataIn = slave Flow Vec(complexType, N)

  def butterflyRealR(data: Seq[SFix]): Seq[SFix] = fold(data).map { case (d, d1) => (d + d1) } ++ fold(data).map { case (d, d1) => (d - d1) }

  def butterflyComplexR(data: Seq[ComplexNumber]): Seq[ComplexNumber] = fold(data).map { case (d, d1) => (d + d1) } ++ fold(data).map { case (d, d1) => (d - d1) }

  def swapR(data: Seq[ComplexNumber]): Seq[SFix] = {
    val reals = data.map(_.real)
    val imags = data.map(complex => -complex.imag)
    reals ++ imags
  }

  val stageMax = log2Up(N)
  implicit val multLatency = 2

  def fig1R(stack: Stack[ComplexNumber], stage: Int): Seq[ComplexNumber] = {
    if (stage == stageMax) butterflyComplexR(Seq(stack.pop(), stack.pop()))
    else {
      val prev0 = fig1R(stack, stage + 1)
      val prev1 = fig1R(stack, stage + 1)
      val indices = (0 until prev1.length).map(_ * 1 << (stage - 1))
      val multiplied = prev1.zip(indices).map { case (complex, i) => multiplyWNnk(complex, coeffType, -i, N) }
      val delayed = Delay(Vec(prev0), multLatency)
      val combined = delayed ++ multiplied
      butterflyComplexR(combined)
    }
  }

  def fig2R(stack: Stack[ComplexNumber], stage: Int): Seq[SFix] = {
    if (stage == stageMax) butterflyRealR(Seq(stack.pop(), stack.pop()).map(_.real))
    else {
      val prev0 = fig2R(stack, stage + 1)
      val prev1 = if (stage + 2 <= stageMax) fig1R(stack, stage + 2) else Seq(stack.pop())
      val indices = (0 until prev1.length).map(_ * 1 << (stage - 1))
      val multiplied = prev1.zip(indices).map { case (complex, i) => multiplyWNnk(complex, coeffType, -i, N) }
      val swapped = swapR(multiplied)
      val delayed = Delay(Vec(prev0), multLatency)
      butterflyRealR(delayed ++ swapped)
    }
  }

  val reverseIndices = (0 until N).map(bitReverse(N, _))
  val droppedIndices = (0 until N).filter { i =>
    val up = 1 << log2Up(i + 1)
    val level = up / 4
    level != 0 && (i / level) % 4 == 3
  }.map(bitReverse(N, _))

  val remainedIndices = reverseIndices.filterNot(droppedIndices.contains(_))
  val remainedInput = remainedIndices.map(dataIn.payload.apply(_))
  val dataStack = Stack(remainedInput: _*)

  val ret = Vec(fig2R(dataStack, 1))
  val dataOut = master Flow Vec(HardType(ret.head), N)
  dataOut.payload.foreach(_.raw.simPublic()) // FIXME: necessary, but why?
  dataOut.payload := ret

  def latency = LatencyAnalysis(dataIn.payload.head.real.raw, dataOut.payload.head.raw)

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}

object R2HSIFFT {
  def main(args: Array[String]): Unit = {
    GenRTL(R2HSIFFT(64, HardType(SFix(7 exp, -8 exp)), HardType(SFix(1 exp, -11 exp))))
  }
}

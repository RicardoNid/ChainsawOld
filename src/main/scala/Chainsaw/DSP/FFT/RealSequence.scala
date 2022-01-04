package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

/** preprocess for real sequence dft
 *
 * @see [[algos.Dft.rvdftByDouble]]
 */
case class RVPreprocess(N: Int, dataType: HardType[SFix])
  extends Component with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {
  val complexType = HardType(ComplexNumber(dataType()))
  override val dataIn = slave Stream Vec(dataType(), N)
  override val dataOut = master Stream Vec(complexType(), N)
  override val latency = 1

  val STEP0 = RegInit(True) // at step 0, get and store data1
  when(dataIn.fire)(STEP0 := ~STEP0)
  val STEP1 = ~STEP0

  dataIn.ready := True
  dataOut.valid := STEP1 && dataIn.fire

  val dataRead = RegNextWhen(dataIn.payload, STEP0)

  dataOut.payload.zip(dataRead).foreach { case (complex, fix) => complex.real := fix }
  dataOut.payload.zip(dataIn.payload).foreach { case (complex, fix) => complex.imag := fix }
}

// TODO: outputs(and calculations) can be reduced by half as the output is hermitian symmetric
// TODO: besides, there exists patterns to be utilized in calculations

/** postprocess for real sequence dft
 *
 * @see [[algos.Dft.rvdftByDouble]]
 */
case class RVPostprocess(N: Int, dataType: HardType[SFix], fold: Int = 1)
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {
  val complexType = HardType(ComplexNumber(dataType()))
  override val dataIn = slave Stream Vec(complexType(), N)
  override val dataOut = master Stream Vec(complexType(), N)
  override val latency = 1

  def symmetricOf(data: Vec[ComplexNumber]) = (data.head +: data.tail.reverse).map(_.conj)

  val STEP0 = RegInit(True) // at step 0, get and store data, while calculating output0
  val STEP1 = ~STEP0
  when(dataIn.fire || STEP1)(STEP0 := ~STEP0)

  val data = dataIn.payload
  val dataRegs = RegNextWhen(data, STEP0)
  val outputRegs = Reg(cloneOf(dataOut.payload))

  val dataSymmetric = symmetricOf(data)
  val dataRegSymmetric = symmetricOf(dataRegs)

  when(STEP0)(outputRegs := Vec(data.zip(dataSymmetric).map { case (a, b) => ((a + b) >> 1).truncated(dataType) }))
    .otherwise(outputRegs := Vec(dataRegs.zip(dataRegSymmetric).map { case (a, b) => ((a.divideI - b.divideI) >> 1).truncated(dataType) }))

  dataIn.ready := STEP0
  dataOut.valid := STEP1 || RegNext(STEP1, init = False)
  dataOut.payload := outputRegs
}


/** preprocess for hermitian symmetric sequence idft
 *
 * @see [[algos.Dft.rvidftByDouble]]
 */
case class HSPreprocess(N: Int, dataType: HardType[SFix])
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {
  val complexType = HardType(ComplexNumber(dataType()))
  override val dataIn = slave Stream Vec(complexType(), N)
  override val dataOut = master Stream Vec(complexType(), N)
  override val latency = 1

  val STEP0 = RegInit(True) // at step 0, get and store data1
  when(dataIn.fire)(STEP0 := ~STEP0)
  val STEP1 = ~STEP0

  val dataRegs = RegNextWhen(dataIn.payload, STEP0)

  dataIn.ready := True
  dataOut.valid := STEP1 && dataIn.fire
  dataOut.payload.zip((dataRegs).zip(dataIn.payload))
    .foreach { case (ret, (a, b)) => ret := a + b.multiplyI }
}


/** postprocess for hermitian symmetric sequence idft
 *
 * @see [[algos.Dft.rvidftByDouble]]
 */
case class HSPostprocess(N: Int, dataType: HardType[SFix])
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[SFix]] {
  val complexType = HardType(ComplexNumber(dataType()))
  override val dataIn = slave Stream Vec(complexType(), N)
  override val dataOut = master Stream Vec(dataType(), N)
  override val latency = 0

  val STEP0 = RegInit(True) // at step 0, get and store data, while calculating output0
  when(dataIn.fire || ~STEP0)(STEP0 := ~STEP0)
  val STEP1 = ~STEP0

  val data = dataIn.payload
  val dataReal = Vec(data.map(_.real))
  val imagRegs = RegNextWhen(Vec(data.map(_.imag)), STEP0)

  when(STEP0)(dataOut.payload := dataReal)
    .otherwise(dataOut.payload := imagRegs)

  dataIn.ready := STEP0
  dataOut.valid := dataIn.fire || ~STEP0
}
package Chainsaw.DFG

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber): GeneralNode[T] = GeneralNode(hardware, name, delay, exeTime)
  }

  /** An operator which won't output the input, whether it is of hardware or software
   */
  def Line[T](width: BitCount = -1 bits): DSPHardware[T] = DSPHardware(
    impl = (dataIns: Seq[T], _: GlobalCount) => dataIns,
    inDegree = 1,
    outWidths = Seq(width)
  )

  def sIntConst(constant: Int, width: BitCount) = DSPHardware(
    (_: Seq[SInt], _: GlobalCount) => Seq(S(constant, width)),
    1,
    Seq(width))

  class SIntConst(name: String, val constant: Int, width: BitCount) extends
    ConstantNode(sIntConst(constant, width), name) with Foldable[SInt]{
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = {
      val constants = sources.map(_.asInstanceOf[SIntConst].constant)
      val foldedFunction = (_: Seq[SInt], _: GlobalCount) => Seq(sintCoeffROM(constants, width))
      val foldedHardware = DSPHardware[SInt](foldedFunction, 1, Seq(width))
      ConstantNode(foldedHardware,  s"foldFrom${sources.head.name}")
    }
  }

  object SIntConst {
    def apply(name: String, constant: Int, width: BitCount): SIntConst = new SIntConst(name, constant, width)
  }


  def sIntInc(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay(dataIns(0) + 1, delay.toInt, init = dataIns.head.getZero)),
    1,
    Seq(width))

  def sIntMult(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay(dataIns(0) * dataIns(1), delay.toInt, init = dataIns.head.getZero)),
    2,
    Seq(width))

  class SIntMult(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode(sIntMult(width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = SIntAdder(s"foldFrom${sources.head.name}", width, delay, exeTime)
  }

  object SIntMult{
    def apply(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber = 1 ns): SIntMult = new SIntMult(name, width, delay, exeTime)
  }

  // binary adder
  def sIntAdder(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay(dataIns(0) + dataIns(1), delay.toInt, init = dataIns.head.getZero)),
    2,
    Seq(width))

  class SIntAdder(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode(sIntAdder(width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = SIntAdder(s"foldFrom${sources.head.name}", width, delay, exeTime)
  }

  object SIntAdder {
    def apply(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber = 1 ns): SIntAdder = new SIntAdder(name, width, delay, exeTime)
  }

  def sIntCAdder(constant: Int, width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay(dataIns(0) + constant, delay.toInt, init = dataIns.head.getZero)),
    1,
    Seq(width)
  )

  class SIntCAdder(name: String, val constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode(sIntCAdder(constant, width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = {
      val constants = sources.map(_.asInstanceOf[SIntCAdder].constant)
      val foldedFunction = (dataIns: Seq[SInt], globalCount: GlobalCount) =>
        Seq(Delay((dataIns(0) + sintCoeffROM(constants, width)), delay.toInt, init = dataIns.head.getZero))
      val foldedHardware = DSPHardware(foldedFunction, 1, Seq(width))
      GeneralNode(foldedHardware, s"foldFrom${sources.head.name}", delay, exeTime)
    }
  }

  object SIntCAdder {
    def apply(name: String, constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber): SIntCAdder = new SIntCAdder(name, constant, width, delay, exeTime)
  }

  def sIntCMulAdder(constant: Int, width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay((dataIns(0) * constant).resized + dataIns(1), delay.toInt, init = dataIns.head.getZero)),
    2,
    Seq(width)
  )

  class SIntCMulAdder(name: String, val constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode(sIntCMulAdder(constant, width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = {
      val constants = sources.map(_.asInstanceOf[SIntCMulAdder].constant)
      val foldedFunction = (dataIns: Seq[SInt], _: GlobalCount) =>
        Seq(Delay((dataIns(0) * sintCoeffROM(constants, width)).resized + dataIns(1), 3, init = dataIns.head.getZero))
      val foldedHardware = DSPHardware(foldedFunction, 1, Seq(width))
      GeneralNode(foldedHardware, s"foldFrom${sources.head.name}", 3 cycles, exeTime)
    }
  }

  object SIntCMulAdder {
    def apply(name: String, constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber): SIntCMulAdder = new SIntCMulAdder(name, constant, width, delay, exeTime)
  }

  def sIntAdderC(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => { // dataIns(2) is the carry
      val full = Delay(dataIns(0) +^ dataIns(1) + dataIns(2), delay.toInt, init = dataIns.head.getZero.resize(dataIns.head.getBitsWidth + 1))
      Seq(full(full.getBitsWidth - 2 downto 0), full.msb.asSInt)
    },
    3,
    Seq(width, 1 bits))

  class SIntAdderC(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode(sIntAdderC(width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = SIntAdderC(s"foldFrom${sources.head.name}", width, delay, exeTime)
  }

  object SIntAdderC {
    def apply(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber): SIntAdderC = new SIntAdderC(name, width, delay, exeTime)
  }

  // constant multiplier
  def sIntCMult(constant: Int, width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay((dataIns(0) * constant).resize(dataIns(0).getBitsWidth), delay.toInt, init = dataIns.head.getZero)),
    1,
    Seq(width))

  def sintCoeffROM(constants: Seq[Int], width: BitCount) = {
    val ROM = Mem(constants.map(S(_, width)))
    val localCounter = CounterFreeRun(constants.size)
    localCounter.setName("ROMCounter")
    ROM.readAsync(localCounter.value)
  }

  class SIntCMult(name: String, val constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber) extends
    GeneralNode(sIntCMult(constant, width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = {
      val constants = sources.map(_.asInstanceOf[SIntCMult].constant)
      val foldedFunction = (dataIns: Seq[SInt], _: GlobalCount) =>
        Seq(Delay((dataIns(0) * sintCoeffROM(constants, width)).resize(dataIns(0).getBitsWidth), 2, init = dataIns.head.getZero))
      val foldedHardware = DSPHardware(foldedFunction, 1, Seq(width))
      GeneralNode(foldedHardware, s"foldFrom${sources.head.name}", 2 cycles, exeTime)
    }
  }

  object SIntCMult {
    def apply(name: String, constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber): SIntCMult = new SIntCMult(name, constant, width, delay, exeTime)
  }

  // for simulation(using delay)
  def sintKeep = DSPHardware((dataIns: Seq[SInt], _: GlobalCount) => Seq(dataIns(0)), 1, Seq(-1 bits))

  def sintKeep(width: BitCount) = DSPHardware((dataIns: Seq[SInt], _: GlobalCount) => Seq(dataIns(0)), 1, Seq(width))

  def bitsKeep = DSPHardware((dataIns: Seq[Bits], _: GlobalCount) => Seq(dataIns(0)), 1, Seq(-1 bits))

  def WNnk(N: Int, nk: Int): MComplex = {
    import scala.math.{Pi, cos, sin}
    val phase = -2 * Pi * nk / N
    MComplex(cos(phase), sin(phase))
  }

  // TODO: implement this as an operator
  def multiplyWNnk(signal: ComplexNumber, index: Int, N: Int,
                   dataType: HardType[SFix], coeffType: HardType[SFix])(implicit latency: Int = 2): ComplexNumber = {

    implicit class sfixPipeline[T <: Data](sf: T) {
      def pipelined(implicit doPipeline: Boolean) = if (doPipeline) RegNext(sf) else sf
    }
    implicit var doPipeline = false


    def delayed(signal: ComplexNumber) = Delay(signal, latency)

    def toFixedCoeff: Double => SFix = SF(_, coeffType().maxExp exp, coeffType().minExp exp)

    // multiply (1 - j) / sqrt(2)
    def multiply1minusj(signal: ComplexNumber) = {
      val A = signal.real + signal.imag
      val B = signal.imag - signal.real
      doPipeline = latency > 1
      val fullReal = A.pipelined * toFixedCoeff(1 / scala.math.sqrt(2.0))
      val fullImag = B.pipelined * toFixedCoeff(1 / scala.math.sqrt(2.0))
      doPipeline = latency > 0
      val fullComplex = ComplexNumber(fullReal.pipelined, fullImag.pipelined)
      doPipeline = latency > 2
      fullComplex.truncated(dataType).pipelined
    }

    // trivial values
    val trivialValue = if (N % 8 == 0 && index % (N / 8) == 0) index / (N / 8) else -1
    trivialValue match {
      case 0 => delayed(signal)
      case 2 => delayed(-signal.multiplyI)
      case 4 => delayed(-signal)
      case 6 => delayed(signal.multiplyI)

      case 1 => multiply1minusj(signal)
      case 3 => multiply1minusj(-signal.multiplyI)
      case 5 => multiply1minusj(-signal)
      case 7 => multiply1minusj(signal.multiplyI)

      case _ => { // nontrivial values
        val coeffValue = WNnk(N, index)
        val coeff = ComplexNumber(toFixedCoeff(coeffValue.real), toFixedCoeff(coeffValue.imag))
        val fullComplex = signal * coeff
        fullComplex.truncated(dataType)
      }
    }
  }
}

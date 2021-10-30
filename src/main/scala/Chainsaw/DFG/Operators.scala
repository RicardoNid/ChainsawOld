package Chainsaw.DFG

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber): GeneralNode[T] = GeneralNode(hardware, name, delay, exeTime)
  }

  val and: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a & b
  val or: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a | b
  val xor: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a ^ b

  val sintAdd: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a + b
  val sintMult: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a * b
  val sintMultAdd:(SInt , SInt , SInt) => SInt = (a: SInt , b: SInt , c: SInt) => (a * b).resized + c

  def sintMACDSP(delay: Int): (SInt, SInt, SInt) => SInt = (a: SInt, b: SInt, c: SInt) => {
    val ret = if (delay == 0) a * b + c else RegNext(a * b) + c
    ret.addAttribute("use_dsp", "yes")
    ret
  }

  /** An operator which won't output the input, whether it is of hardware or software
   */
  def Line[T](width: BitCount = -1 bits): DSPHardware[T] = DSPHardware(
    impl = (dataIns: Seq[T], _: GlobalCount) => dataIns,
    inDegree = 1,
    outWidths = Seq(width)
  )


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
  /*
  def sintCoeffROM(constants: Seq[Int], width: BitCount) = {
    val ROM = Mem(constants.map(S(_, width)))
    val localCounter = CounterFreeRun(constants.size)
    localCounter.setName("ROMCounter")
    ROM.readAsync(localCounter.value)
  }
  */


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
    val trivialValue = if (N % 8 == 0 && index % (N / 8) == 0) index / (N / 8) else -114
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

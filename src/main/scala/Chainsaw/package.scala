import Chainsaw.dspTest.DSPTestable
import Chainsaw.examples.JsonExample.temp
import Chainsaw.matlabIO._
import org.scalatest.Tag
import spinal.core._
import spinal.core.internals.{BaseNode, DeclarationStatement, GraphUtils, PhaseContext, PhaseNetlist}
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import xilinx.{IMPL, VivadoFlow}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.sys.process.Process
import scala.util.{Failure, Random, Success, Try}
import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.commons.io.FileUtils
import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import scala.io.Source
import scala.reflect.ClassTag

package object Chainsaw extends RealFactory {

  /*
  * following methods are designed for Real type*/

  case class coreHardware() extends scala.annotation.StaticAnnotation


  // TODO: extract the features of the following annotations and implement them in traits

  /** indicating that a module is fully-pipelined, thus, it requires no control at all, its behaviors is fully described by its latency
   *
   */
  case class fullyPipelined() extends scala.annotation.StaticAnnotation

  /** indicating that a module is periodic, thus, it is controlled by an inner counter which is driven by dataIn.fire,
   * its behaviors is fully described by its latency and period
   *
   */
  case class partiallyPipelined() extends scala.annotation.StaticAnnotation

  /** indicating that a module has a registered output, which would not contribute to the critical path of the module it drives
   *
   */
  case class outputRegistered() extends scala.annotation.StaticAnnotation

  import org.slf4j.{LoggerFactory, Logger}

  val logger = LoggerFactory.getLogger("Chainsaw logger")

  var ChainsawNumericDebug = false
  var ChainsawExpLowerBound = -65536

  def printlnWhenNumericDebug(content: Any): Unit = if (ChainsawNumericDebug) printlnYellow(content)

  implicit class numericOp(value: Double) {

    /** Rounding up to the nearest representable value
     *
     * @param ulp
     */
    def roundUp(implicit ulp: Double) = ceil(value / ulp) * ulp

    def roundDown(implicit ulp: Double) = floor(value / ulp) * ulp

    def roundAsScala(implicit ulp: Double) = (value / ulp).toInt * ulp

    def roundAsScalaInt(implicit ulp: Double) = (value / ulp).toInt
  }

  case class ErrorNumber(value: Double)

  implicit class MoreDoubleBuilder(value: Double) {

    def err = ErrorNumber(value)
  }

  implicit class SpinalLiterals(private val sc: StringContext) {

    /** Invoke QFormatParser and make using QFormat easier
     *
     * @example val SF1Q3 = HardType(SFix("1Q3"))
     */
    def Q(args: Any*): QFormat = QFormatParser(getString(args))

    /** Q Format literal, we follow the same definition as Xilinx, where MQN stands for a (M + N + 1) bits signed fixed point number with N fractional bits and M integer bits
     *
     * @see [[https://en.wikipedia.org/wiki/Q_(number_format) Q format]]
     */
    def QFormatParser(string: String): QFormat = {
      val isUnsigned = string.contains("UQ")
      val digits = {
        if (string.contains("SQ")) string.split("SQ").map(_.toInt)
        else if (string.contains("Q")) string.split("Q").map(_.toInt)
        else if (string.contains("UQ")) string.split("UQ").map(_.toInt)
        else throw new IllegalArgumentException("A QFormat must be like 1Q3, 1SQ3, or 1UQ3")
      }

      require(digits.length == 2 && digits.forall(_ >= 0))
      if (isUnsigned) UQ(digits(0) + digits(1), digits(1)) else SQ(digits(0) + digits(1) + 1, digits(1))
    }

    private def getString(args: Any*): String = {

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next())

      while (ai.hasNext) {
        if (ai.hasNext && !ai.next.isInstanceOf[List[_]]) bldr append ai.next
        if (pi.hasNext && !pi.next.isInstanceOf[List[_]]) bldr append pi.next
      }

      bldr.result.replace("_", "")
    }
  }

  //   btToSignal and getDouble are copied from spinal.core.sim package object, as they are private
  private def btToSignal(manager: SimManager, bt: BaseNode) = {
    if (bt.algoIncrementale != -1) {
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation.\n- To fix it, call simPublic() on it durring the elaboration.")
    }

    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }

  //  implicit class MoreBVPimper(bv: BitVector) {
  //    def #=(value: Array[Boolean]) = { //TODO improve perf
  //      var acc = BigInt(0)
  //      value.foreach { bit =>
  //        acc = acc << 1
  //        acc |= (if (bit) 1 else 0)
  //      }
  //      setBigInt(bv, acc)
  //    }
  //
  //    def #=(value: Array[Int]) = { //TODO improve perf
  //      require(value.forall(Array(0, 1).contains(_))) // value should contains only 0 and 1
  //      var acc = BigInt(0)
  //      value.foreach { bit =>
  //        acc = acc << 1
  //        acc |= bit
  //      }
  //      setBigInt(bv, acc)
  //    }
  //  }

  def getDouble(r: Real): Double = {
    if (r.getBitsWidth == 0) return 0
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, r.raw)
    manager.getLong(signal) * scala.math.pow(2, r.minExp)
  }

  implicit class SimRealPimper(r: Real) {

    import r._

    def toDouble = getDouble(r)

    def toBigDecimal = BigDecimal(toDouble)

    def #=(value: BigDecimal): Unit = {
      assert(value <= r.maxValue, s"Literal $value is too big to be assigned in $this")
      assert(value >= r.minValue, s"Literal $value is too small to be assigned in this $this")

      // TODO
      val shift = -r.minExp
      val ret = if (shift >= 0) // ret this is the "binary string" of value at specific precision
        (value * BigDecimal(BigInt(1) << shift)).toBigInt
      else
        (value / BigDecimal(BigInt(1) << -shift)).toBigInt
      setLong(r.raw, ret.toLong)
    }

    def #=(value: Double): Unit = #=(BigDecimal(value))

    private val lowerRepresentable = ceil(lower / ulp).toInt
    //    private val upperRepresentable = {
    //        val temp = floor(upper / ulp).toInt
    //        if (temp > 0) temp - 1 else temp
    //      }
    private val upperRepresentable = floor(upper / ulp).toInt


    /** Generate all possible value under current range, for simulation
     *
     * @example for (-0.3,0.4,0.1) whose ulp = 0.0625, numbers generated are
     */
    def allValues = (lowerRepresentable to upperRepresentable).map(_ * ulp)

    def randomValue() = (Random.nextInt(upperRepresentable - lowerRepresentable + 1) + lowerRepresentable) * ulp

    /** Use a random value in all values for simulation
     *
     */
    def randomize(): Unit = r #= randomValue()

    /** Judge whether the value is "close to" this, that is, the difference is within the error
     */
    def ~=(that: Double) = (r.toDouble - that).abs <= r.error

    def !~=(that: Double) = ! ~=(that)

  }

  implicit class SimRealVectorPimper(rv: Vec[Real]) {

    def #=(value: Seq[Double]): Unit = {
      require(value.length == rv.length, "length of the vector and the stimulus shoul be the same")
      rv.zip(value).foreach { case (real, d) => real #= d }
    }

    def toDouble = rv.map(_.toDouble)

    def toBigDecimal = rv.map(_.toBigDecimal)

    def randomValue = rv.map(_.randomValue())
  }

  /*
  * following methods are used to strengthen built-in simulation utils*/

  /** Simulation methods for signed fixed number
   */
  implicit class SimSFixPimper(sf: SFix) {

    import sf._

    def #=(value: BigDecimal): Unit = {
      assert(value <= maxValue, s"Literal $value is too big to be assigned in $this")
      assert(value >= minValue, s"Literal $value is too small to be assigned in this $this")

      val shift = -minExp
      val ret = if (shift >= 0) // ret this is the "binary string" of value at specific precision
        (value * BigDecimal(BigInt(1) << shift)).toBigInt
      else
        (value / BigDecimal(BigInt(1) << -shift)).toBigInt
      setLong(raw, ret.toLong)
    }

    def #=(value: Double): Unit = #=(BigDecimal(value))

    def toDouble = raw.toBigInt.toDouble / (1 << -minExp)
  }

  /** Simulation methods for complex number, based on SFix
   */
  implicit class SimComplexPimper(cn: ComplexNumber) {
    def #=(value: BComplex): Unit = {
      cn.real #= value.real
      cn.imag #= value.imag
    }

    def toComplex = new BComplex(cn.real.toDouble, cn.imag.toDouble)
  }

  /*
  * following methods/implicit classes are used to make debugging(especially, peek, poke and print) easier */
  var ChainsawDebug = false

  def printlnWhenDebug(content: Any): Unit = if (ChainsawDebug) println(content)

  def printlnColored(content: Any)(color: String) = {
    print(color)
    println(content)
    print(Console.BLACK)
  }

  def printlnRed(content: Any) = printlnColored(content)(Console.RED)

  def printlnYellow(content: Any) = printlnColored(content)(Console.YELLOW)

  def printlnBlue(content: Any) = printlnColored(content)(Console.BLUE)

  def printlnGreen(content: Any) = printlnColored(content)(Console.GREEN)

  /** Methods for BigInt, especially for using it as a bit sequence in simulation and design initialization
   */
  implicit class BigIntUtil(bi: BigInt) {

    // following methods are designed to convert a long bit sequence to multiple segments, making debugging easier

    /** convert a long BitInt to multiple words, each as a new binary String, padded to the left when needed
     *
     * order:
     *
     * inside each element: MSB -> LSB
     *
     * between elements: MSB -> LSB
     */
    def toWordStrings(wordSize: Int, radix: Int = 2): Seq[String] = {
      require(wordSize % log2Up(radix) == 0)
      val wordCount = ceil(bi.toString(radix).length.toDouble / wordSize).toInt
      bi.toString(radix).padToLeft(wordCount * wordSize, '0').grouped(wordSize).toSeq
    }

    def toWordStringsHex: Int => Seq[String] = toWordStrings(_, 16)

    def toWords(wordSize: Int): Seq[BigInt] = bi.toWordStrings(wordSize).map(BigInt(_, 2))

    /** Regard BigInt(from sim) as a binary string and interpret it as 2's complement number
     */
    def toSigned(width: Int) = bi.toString(2).padToLeft(width, '0').toSigned
  }

  /** Utils for Seq, which is the default type we use for collections
   */
  implicit class SeqUtil[T](seq: Seq[T]) {
    def padToLeft(len: Int, elem: T) = seq.reverse.padTo(len, elem).reverse

    def equals(that: Seq[T]) = seq.zip(that).forall { case (t, t1) => t == t1 }

    def approximatelyEquals(that: Seq[T], approEquals: (T, T) => Boolean) = seq.zip(that).forall { case (t, t1) => approEquals(t, t1) }

    def toDv(implicit tag: ClassTag[T]) = new DenseVector(seq.toArray)
  }

  implicit class DvUtil[T](dv: DenseVector[T]) {
    def toSeq(implicit tag: ClassTag[T]): Seq[T] = dv.toArray.toSeq
  }
  //  implicit def Seq2Vec[T <: Data](seq: Seq[T]): Vec[T] = Vec(seq)

  /** An example of approEquals, which has a relative error bound and a absolute error bound
   */
  def doubleEquals(a: Double, b: Double, relative: Double = 0.05, absolute: Double = 0.1) =
    (abs(a - b) / abs((a + b) / 2)) < relative || scala.math.abs(a - b) < absolute

  /** Utils for string which you treat it as a binary number
   */
  implicit class BinaryStringUtil(s: String) {
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse

    def toBigInt = BigInt(s, 2)

    def toUnsigned: BigInt = s.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (BigInt(1) << i) }.sum

    def toSigned: BigInt = s.tail.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (BigInt(1) << i) }.sum - s.head.asDigit * (BigInt(1) << s.length - 1)
  }

  // use this val as Generator to keep the behavior consistency through the project
  val ChainsawRand = new Random(42) // using this as global random gen, with a fixed seed

  import breeze.stats.distributions.Rand

  /** Utils for Random to generate more different types of stimulus
   */
  implicit class RandomUtil(rand: Random) {

    def nextComplex(min: Double = 0, max: Double = 1): BComplex = new BComplex(
      rand.nextDouble() * (max - min) + min,
      rand.nextDouble() * (max - min) + min)

    def nextBigInt(bitLength: Int) = BigInt(rand.nextString(bitLength).map(_ % 2).mkString(""), 2)

    def nextBits(bitLength: Int): Seq[Int] = rand.nextString(bitLength).map(_ % 2)

    def nextBinaryString(bitLength: Int): String = nextBits(bitLength).mkString("")

    def nextComplexDV(size: Int, dist: Rand[Double] = Rand.uniform): DenseVector[BComplex] = {
      val a = DenseVector.rand[Double](size, dist)
      val b = DenseVector.rand[Double](size, dist)
      new DenseVector(a.toArray.zip(b.toArray).map { case (real, imag) => BComplex(real, imag) })
    }
  }

  /*
  * following implicit class are used to strengthen built-in Types for more operations*/
  implicit class SFixUtil(sf: SFix) {
    def unary_-() = {
      val ret = SFix(sf.maxExp exp, sf.minExp exp)
      ret.raw := -sf.raw
      ret
    }

    def doAddSub(that: SFix, add: Boolean) = {
      val (rawLeft, rawRight) = sf.alignLsb(that)
      val ret = SFix(Math.max(sf.maxExp, that.maxExp) + 1 exp, Math.max(rawLeft.getBitsWidth, rawRight.getBitsWidth) + 1 bits)
      ret.raw := (if (add) rawLeft +^ rawRight else rawLeft -^ rawRight)
      ret
    }

    def +^(that: SFix) = doAddSub(that, true)

    def -^(that: SFix) = doAddSub(that, false)

    def abs = {
      val ret = cloneOf(sf)
      ret.raw := sf.raw.abs.asSInt
      ret
    }

    def isPositive = ~sf.raw.msb

    def isNegative = sf.raw.msb

    def truncated(dataType: HardType[SFix]) = {
      val ret = dataType()
      ret := sf.truncated
      ret
    }

    def delay(cycle: Int) = Delay(sf, cycle)
  }

  implicit class VecUtil[T <: Data](vec: Vec[T]) {

    def vecShiftWrapper(bitsShift: UInt => Bits, that: UInt): Vec[T] = {
      val ret = cloneOf(vec)
      val shiftedBits: Bits = bitsShift((that * widthOf(vec.dataType)).resize(log2Up(widthOf(vec.asBits))))
      ret.assignFromBits(shiftedBits)
      ret
    }

    val bits = vec.asBits

    // seems that Xilinx synth can implement this efficiently
    def rotateLeft(that: Int): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateLeft(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateRight(that: Int): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)

    def rotateRight(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)
  }

  implicit class BitsUtil(bits: Bits) {
    def asBoolsKeepOrder: Seq[Bool] = bits.asBools.reverse
  }

  implicit class StreamUtil[Ti <: Data](stream: Stream[Ti]) {
    def payloadMap[To <: Data](transform: Ti => To) = {
      val ret = transform(stream.payload)
      val retStream = Stream(cloneOf(ret))
      retStream.payload := ret
      retStream.valid := stream.valid
      stream.ready := retStream.ready
      retStream
    }

    def withValid[To <: Data](newValid: Bool) = {
      val retStream = cloneOf(stream)
      retStream.payload := stream.payload
      retStream.valid := newValid
      stream.ready := retStream.ready
      retStream
    }

    def withEnable[To <: Data](enable: Bool) = {
      val retStream = cloneOf(stream)
      retStream.payload := stream.payload
      retStream.valid := stream.valid && enable
      stream.ready := retStream.ready && enable
      retStream
    }

  }


  /** following methods are designed for doing synth/impl through command-line
   * for these methods, the results are exported to a default dir in this project, the sub dir can be specific by name field
   */
  def GenRTL[T <: Component](gen: => T, name: String = "temp") = {
    val targetDirectory = s"./elaboWorkspace/$name"
    if (!Files.exists(Paths.get("./elaboWorkspace"))) doCmd("mkdir elaboWorkspace")
    new File(targetDirectory).mkdir()
    val report: SpinalReport[T] = SpinalConfig(netlistFileName = s"$name.v", targetDirectory = targetDirectory).generateVerilog(gen)
    logger.info(report.rtlSourcesPaths
      .map(Paths.get(_))
      .map(path => if (path.isAbsolute) path else path.toAbsolutePath)
      .mkString("\n"))
    logger.info(s"source files generated:\n${report.getRtlString()}")
    val succeed = report.unusedSignals.isEmpty // FIXME: this definition is terrible, but we use it currently
    succeed
  }

  val synthWorkspace = "/home/ltr/IdeaProjects/Chainsaw/synthWorkspace"

  import xilinx._

  import scala.annotation.meta._

  @getter
  @setter
  @beanGetter
  @beanSetter
  class xilinxDevice(message: String = "", since: String = "") extends scala.annotation.StaticAnnotation

  def getDutForTiming[Ti <: Data, To <: Data](gen: => Component with DSPTestable[Ti, To]) =
    new Component { // constructing dut with extra registers
      val core = gen
      val dataIn = in(cloneOf(core.dataIn.payload))
      val dataOut = out(cloneOf(core.dataOut.payload))

      logger.info(s"synth for timing, " +
        s"${core.dataIn.payload.getBitsWidth + core.dataOut.payload.getBitsWidth} extra regs added")

      core.dataIn.valid := True
      core.dataOut match {
        case stream: Stream[_] => stream.ready := True
        case _ =>
      }

      core.dataIn.payload := RegNext(dataIn)
      dataOut := RegNext(core.dataOut.payload)
    }

  def getDutForImpl[Ti <: Data, To <: Data](gen: => Component with DSPTestable[Ti, To]) =
    new Component { // constructing dut with extra registers
      val core = gen

      import Chainsaw.crypto.Lfsr

      val inputWidth = core.dataIn.payload.getBitsWidth
      val poly: Seq[Int] = 1 +: (0 until inputWidth - 1).map(_ => ChainsawRand.nextInt(2)) :+ 1
      val inputGen = Lfsr(poly, vec = true)

      val topOut = out Bool()

      logger.info(s"synth for timing, " +
        s"${core.dataIn.payload.getBitsWidth + core.dataOut.payload.getBitsWidth} extra regs added")

      core.dataIn.valid := True
      core.dataOut match {
        case stream: Stream[_] => stream.ready := True
        case _ =>
      }

      core.dataIn.payload.assignFromBits(RegNext(inputGen.dataOut))
      val xor = (a: Bool, b: Bool) => a ^ b
      val bridge = (a: Bool, i: Int) => if (i % 3 == 2) RegNext(a) else a
      topOut := RegNext(core.dataOut.payload).asBits.asBools.reduceBalancedTree(xor, bridge)
    }


  def VivadoSynth[T <: Component](gen: => T, name: String = "temp") = {
    val report = VivadoFlow(design = gen, taskType = SYNTH, topModuleName = name, workspacePath = s"$synthWorkspace/$name").doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  /** compared with VivadoSynth, this method insert registers before inputs and after outputs, which lead to more accurate timing report
   */
  def VivadoSynthForTiming[Ti <: Data, To <: Data]
  (gen: => Component with DSPTestable[Ti, To], name: String = "temp") = VivadoSynth(getDutForTiming(gen), name)

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp", xdcPath: String = null) = {
    val report = VivadoFlow(design = gen, taskType = IMPL, topModuleName = name, workspacePath = s"$synthWorkspace/$name").doFlow()
    report.printArea()
    report.printFMax()
    report
  }

  def VivadoImplForTiming[Ti <: Data, To <: Data]
  (gen: => Component with DSPTestable[Ti, To], name: String = "temp", xdcPath: String = null) = VivadoImpl(getDutForImpl(gen), name, xdcPath)

  val config0 = ClockDomainConfig(resetKind = ASYNC)
  val config1 = ClockDomainConfig(resetKind = SYNC)

  def RegAsync[T <: Data](dataType: HardType[T]): T = {

    val domain0 = ClockDomain.external("domain0", config0)

    val area = new ClockingArea(domain0) {
      val ret = Reg(dataType)
    }
    area.ret
  }

  def RegSync[T <: Data](dataType: HardType[T]): T = {

    val domain1 = ClockDomain.external("domain1", config1)

    val area = new ClockingArea(domain1) {
      val ret = Reg(dataType)
    }
    area.ret
  }

  def nextMultiple(value: Int, base: Int) = {
    (ceil(value.toDouble / base) * base).toInt
  }

  private def doCmd(cmd: String): Unit = {
    println(cmd)
    Process(cmd) !
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    val (p, q) = if (a >= b) (a, b) else (b, a)
    if (q == 0) p
    else gcd(q, p % q)
  }

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  object DFGTest extends Tag("DFGTest")

  type BComplex = breeze.math.Complex

  object BComplex {
    def apply(real: Double, imag: Double): BComplex = new BComplex(real, imag)

    def apply(real: Double): BComplex = new BComplex(real, 0)
  }

  /** Implement some basic operations of complex number
   *
   * @param complex
   */
  implicit class ComplexUtil(complex: BComplex) {

    def toMComplex = new MComplex(complex.real, complex.imag)

    def formatted(fmtstr: String) = complex.real.formatted(fmtstr) + " + " + complex.imag.formatted(fmtstr) + "i"

    def sameAs(that: BComplex, epsilon: Double = 1.0) = {
      (complex.real - that.real).abs < epsilon &&
        (complex.imag - that.imag).abs < epsilon
    }

    override def equals(obj: Any) = {
      obj match {
        case complex: BComplex => this.sameAs(complex)
        case _ => false
      }
    }

    def toString(length: Int) =
      complex.real.toString.padTo(length, ' ').take(length) + " + " +
        (complex.imag.toString).padTo(length, ' ').take(length) + "i"

    def modulus = scala.math.sqrt(complex.real * complex.real + complex.imag * complex.imag)

    def unary_- = new BComplex(-complex.real, -complex.imag)
  }

  // all these methods are from lower to higher(the same as subdividein)
  def toWords(value: BigInt, wordLength: Int, wordCount: Int): Seq[BigInt] = toWordStrings(value, wordLength, wordCount).map(BigInt(_, 2))

  def toWordStrings(value: BigInt, wordLength: Int, wordCount: Int): Seq[String] = {
    value.toString(2).padToLeft(wordLength * wordCount, '0')
      .grouped(wordLength).toSeq.takeRight(wordCount).reverse
  }

  def toWordsHexString(value: BigInt, w: Int, e: Int): String =
    toWords(value, w, e).map(_.toString(16).padToLeft(w / 4, '0') + " ").flatten.mkString("")

  def SFLike(value: Double, sf: SFix) = {
    val ret = cloneOf(sf)
    ret := value
    ret
  }

  def SFLike(value: Double, hardType: HardType[SFix]) = {
    val ret = cloneOf(hardType())
    ret := value
    ret
  }

  def toComplexType(fixType: HardType[SFix]) = HardType(ComplexNumber(fixType))

  implicit class ArrayMatrixUtil[T](array2D: Array[Array[T]])(implicit tag: ClassTag[T]) {

    def rows = array2D.length

    def columns = array2D.head.length

    def transposed = Array.tabulate(columns, rows)((j, i) => array2D(i)(j))

    def toDM = new DenseMatrix[T](array2D.rows, array2D.columns, array2D.transpose.flatten)
  }

  implicit class DMUtil[T](dm: DenseMatrix[T])(implicit tag: ClassTag[T]) {

    def toRowMajoredArray = dm.t.toArray

    def to2DArray = toRowMajoredArray.grouped(dm.cols).toArray
  }

  implicit var cmultConfig = ComplexMultConfig() // configuration of complex multiplication
}


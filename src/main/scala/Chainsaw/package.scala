import Chainsaw.matlabIO._
import spinal.core._
import spinal.core.internals.BaseNode
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import xilinx.{IMPL, VivadoFlow, VivadoTask}

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.sys.process.Process
import scala.util.Random

package object Chainsaw extends RealFactory {

  /*
  * following methods are designed for Real type*/
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
    def #=(value: MComplex): Unit = {
      cn.real #= value.real
      cn.imag #= value.imag
    }

    def toComplex = new MComplex(cn.real.toDouble, cn.imag.toDouble)
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
  val DSPRand = new Random(42) // using this as global random gen, with a fixed seed

  /** Utils for Random to generate more different types of stimulus
   */
  implicit class RandomUtil(rand: Random) {

    def nextComplex(min: Double = 0, max: Double = 1) = new MComplex(
      rand.nextDouble() * (max - min) + min,
      rand.nextDouble() * (max - min) + min)

    def nextBigInt(bitLength: Int) = BigInt(rand.nextString(bitLength).map(_ % 2).mkString(""), 2)

    def nextBinaryString(bitLength: Int): String = rand.nextString(bitLength).map(_ % 2).mkString("")
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

  implicit class StreamUtil[T <: Data](stream: Stream[T]) {
    def >=>(that: Stream[T]): Unit = {

    }
  }

  /*
  * following methods are designed for doing synth/impl through command-line
  * for these methods, the results are exported to a default dir in this project, the sub dir can be specific by name field*/
  def GenRTL[T <: Component](gen: => T, print: Boolean = false, name: String = "temp") = {
    val targetDirectory = s"./elaboWorkspace/$name"
    if (!Files.exists(Paths.get("./elaboWorkspace"))) doCmd("mkdir elaboWorkspace")
    new File(targetDirectory).mkdir()
    val report = SpinalConfig(netlistFileName = s"$name.sv", targetDirectory = targetDirectory).generateSystemVerilog(gen)
    println(report.rtlSourcesPaths
      .map(Paths.get(_))
      .map(path => if (path.isAbsolute) path else path.toAbsolutePath)
      .mkString("\n"))
    if (print) println(report.getRtlString())
  }

  val synthWorkspace = "/home/ltr/IdeaProjects/Chainsaw/synthWorkspace"

  def VivadoSynth[T <: Component](gen: => T, name: String = "temp") = {
    val report = VivadoFlow(design = gen, name, s"synthWorkspace/$name").doit()
    report.printArea()
    report.printFMax()
    report
  }

  def VivadoSynth[T <: Component](source: String): Unit = {
    val report = VivadoFlow(design = Chainsaw.examples.ZyboDesign0(), "temp", s"synthWorkspace/temp", designPath = source).doit()
    report.printArea()
    report.printFMax()
  }

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp", xdcPath: String = "") = {
    val report = VivadoFlow(design = gen, name, s"synthWorkspace/$name", vivadoTask = VivadoTask(taskType = IMPL)).doit()
    report.printArea()
    report.printFMax()
    report
  }

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

  def gcd(a: Int, b: Int):Int = {
    require(a >= b)
    if (b == 0) a
    else gcd(b, a % b)
  }

  def lcm(a: Int, b: Int) = a * b / gcd(a, b)
}
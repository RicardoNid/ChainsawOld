import breeze.linalg.DenseVector
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.core.internals.BaseNode
import spinal.core.sim._
import spinal.lib.fsm.{State, StateCompletionTrait, StateMachineAccessor, StateMachineSharableRegUInt, StateMachineSharableUIntKey}
import spinal.sim._
import xilinx.{ELABO, IMPL, VivadoFlow, VivadoTask}

import java.nio.file.Paths
import scala.collection.mutable.ArrayBuffer
import scala.math.{BigInt, ceil, floor, pow}
import scala.util.Random

package object Chainsaw extends RealFactory {

  // methods for padding
  // example: YWords = toWords(BigInt("1_1111_0000", 4, 3)), YWords(0) = 0, YWords(1) = 15, YWords(2) = 1(padded as 0001)
  def toWords(value: BigInt, w: Int, e: Int) = {
    value.toString(2).padToLeft(e * w, '0')
      .grouped(w).toArray.takeRight(e).map(BigInt(_, 2))
      .reverse
  }

  def toWordStrings(value: BigInt, wordSize: Int, wordCount: Int) = {
    value.toString(2).padToLeft(wordSize * wordCount, '0')
      .grouped(wordSize).toSeq.takeRight(wordCount)
  }

  // from lsb to msb
  def toWordsHexString(value: BigInt, w: Int, e: Int) =
    toWords(value, w, e).map(_.toString(16).padToLeft(w / 4, '0') + " ").flatten.mkString("")

  // print the padded number in hex form, so it appears the same as in gtkwave
  def printPadded(name: String, value: BigInt, n: Int): Unit = {
    require(n % 4 == 0)
    val hex =
      value.toString(2).padToLeft(n, '0')
        .grouped(4).toArray.map(BigInt(_, 2).toString(16))
        .mkString("")
    println(s"$name = $hex")
  }

  class StateDelayFixed(cyclesCount: UInt)(implicit stateMachineAccessor: StateMachineAccessor) extends State with StateCompletionTrait {

    /** Create a StateDelay with an TimeNumber */
    def this(time: TimeNumber)(implicit stateMachineAccessor: StateMachineAccessor) {
      this((time * ClockDomain.current.frequency.getValue).toBigInt)
    }

    val cache = stateMachineAccessor.cacheGetOrElseUpdate(StateMachineSharableUIntKey, new StateMachineSharableRegUInt).asInstanceOf[StateMachineSharableRegUInt]
    cache.addMinWidth(cyclesCount.getWidth)

    whenIsActive {
      cache.value := cache.value - 1
      when(cache.value <= 1) {
        doWhenCompletedTasks()
      }
    }

    onEntry {
      cache.value := cyclesCount.resized
    }

    whenIsNext {
      when(cache.value <= 1) {
        cache.value := cyclesCount.resized
      }
    }
  }

  implicit class MoreOnString(s: String) {
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse
  }

  // btToSignal and getDouble are copied from spinal.core.sim package object, as they are private
  private def btToSignal(manager: SimManager, bt: BaseNode) = {
    if (bt.algoIncrementale != -1) {
      SimError(s"UNACCESSIBLE SIGNAL : $bt isn't accessible during the simulation.\n- To fix it, call simPublic() on it durring the elaboration.")
    }

    manager.raw.userData.asInstanceOf[ArrayBuffer[Signal]](bt.algoInt)
  }

  def getDouble(r: Real): Double = {
    if (r.getBitsWidth == 0) return 0
    val manager = SimManagerContext.current.manager
    val signal = btToSignal(manager, r.raw)
    manager.getLong(signal) * scala.math.pow(2, r.minExp)
  }

  import spinal.core.sim

  implicit class MoreBVPimper(bv: BitVector) {
    def #=(value: Array[Boolean]) = { //TODO improve perf
      var acc = BigInt(0)
      value.foreach { bit =>
        acc = acc << 1
        acc |= (if (bit) 1 else 0)
      }
      setBigInt(bv, acc)
    }

    def #=(value: Array[Int]) = { //TODO improve perf
      require(value.forall(Array(0, 1).contains(_))) // value should contains only 0 and 1
      var acc = BigInt(0)
      value.foreach { bit =>
        acc = acc << 1
        acc |= bit
      }
      setBigInt(bv, acc)
    }
  }

  implicit class SimSFixPimper(sf: SFix){
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

    def toDouble() = raw.toBigInt.toDouble / (1 << -minExp)
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
    def randomize() = r #= randomValue()

    /** Judge whether the value is "close to" this, that is, the difference is within the error
     *
     * @param that
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

  // debug mode
  var ChainsawDebug = false
  var ChainsawNumericDebug = false
  // ronding mode
  var ChainsawExpLowerBound = -65536

  import com.mathworks.engine.MatlabEngine

  lazy val eng = try {
    println(s"Matlab Engine Started")
    MatlabEngine.startMatlab
  }

  def printlnWhenNumericDebug(content: Any) = if (ChainsawNumericDebug) printlnYellow(content)

  def printlnWhenDebug(content: Any) = if (ChainsawDebug) println(content)

  def printlnColored(content: Any)(color: String) = {
    print(color)
    println(content)
    print(Console.BLACK)
  }

  def printlnGreen(content: Any) = printlnColored(content)(Console.GREEN)

  def printlnRed(content: Any) = printlnColored(content)(Console.RED)

  def printlnYellow(content: Any) = printlnColored(content)(Console.YELLOW)

  def MySFix(maxValue: Double, minValue: Double, resolution: Double): SFix = {
    require(maxValue >= 0)
    val maxExp0 = log2Up(floor(maxValue + 1).toInt)
    val maxExp1 = log2Up(abs(minValue).toInt)
    val maxExp = Array(maxExp0, maxExp1).max
    val minExp = -log2Up(abs(1 / resolution).toInt)
    SFix(maxExp exp, minExp exp)
  }

  def MySFix(maxValue: Double, resolution: Double): SFix = MySFix(maxValue, -maxValue, resolution)

  /** SFix literal with an appropriated bitWidth
   *
   * @param value
   * @param resolution
   * @return
   */
  def MySF(value: Double, resolution: Double = 1.0) = {
    val tmp = MySFix(value, resolution)
    tmp := value
    tmp
  }

  // typedefs and numeric considerations
  val naturalWidth = 6
  val fractionalWidth = 10
  val bitWidth = naturalWidth + fractionalWidth

  def phaseType(resolution: Double = 0.001) = MySFix(Pi, -Pi, resolution) // [-Pi, Pi] for phase

  def unitType(resolution: Double = 0.001) = MySFix(1, -1, resolution)

  def globalType = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  def shortGlobalType = SFix((naturalWidth / 2) exp, -(fractionalWidth / 2) exp)

  val testFFTLength = 8

  def sameFixed(a: Double, b: Double) = (abs(a - b) / abs((a + b) / 2)) < 0.05 || scala.math.abs(a - b) < 0.1

  def sameFixedSeq(v1: IndexedSeq[Double], v2: IndexedSeq[Double]) =
    v1.zip(v2).forall { case (c1, c2) => sameFixed(c1, c2) }

  def sameFixedVector(v1: DenseVector[Double], v2: DenseVector[Double]) = sameFixedSeq(v1.toArray, v2.toArray)

  //  def Double2Fix(value: Double) = floor(value * (1 << 4)).toInt // convert Double to valid stimulus for simulation
  //  def Fix2Double(value: SFix) = value.raw.toBigInt.toDouble / pow(2, 4)
  def Double2Fix(value: Double, fw: Int = fractionalWidth) = floor(value * (1 << fw)).toInt // convert Double to valid stimulus for simulation

  def Fix2Double(value: SFix, fw: Int = fractionalWidth) = value.raw.toBigInt.toDouble / pow(2, fw)

  // OPTIMIZE: implement prime & factor by table

  val DSPRand = new Random(42) // using this as global random gen, with a fixed seed

  def bs2i(bs: String) = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum

  def bs2i2c(bs: String) = {
    val values = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }
    values.dropRight(1).sum - values.last
  }

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

  def GenRTL[T <: Component](gen: => T, print: Boolean = false, name: String = "temp") = {
    val report = SpinalConfig(netlistFileName = s"$name.sv").generateSystemVerilog(gen)
    println(report.rtlSourcesPaths
      .map(Paths.get(_))
      .map(path => if (path.isAbsolute) path else path.toAbsolutePath)
      .mkString("\n"))
    if (print) println(report.getRtlString())
  }

  def VivadoSynth[T <: Component](gen: => T, name: String = "temp"): Unit = {
    val report = VivadoFlow(design = gen, name, s"synthWorkspace/$name").doit()
    report.printArea()
    report.printFMax()
  }

  def VivadoImpl[T <: Component](gen: => T, name: String = "temp"): Unit = {
    val report = VivadoFlow(design = gen, name, s"synthWorkspace/$name", vivadoTask = VivadoTask(taskType = IMPL)).doit()
    report.printArea()
    report.printFMax()
  }

  def VivadoElabo[T <: Component](gen: => T): Unit = {
    val report = VivadoFlow(design = gen, "temp", "synthWorkspace/temp", vivadoTask = VivadoTask(taskType = ELABO)).doit()
    report.printArea()
    report.printFMax()
  }

  val synthWorkspace = "/home/ltr/IdeaProjects/Chainsaw/synthWorkspace"

  implicit class SpinalLiterals(private val sc: StringContext) {

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

    def Q(args: Any*): QFormat = QFormatParser(getString(args))

    // TODO: find out how it works
    private def getString(args: Any*): String = {

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(pi.next().toString)

      while (ai.hasNext) {
        if (ai.hasNext && !ai.next.isInstanceOf[List[_]]) bldr append ai.next
        if (pi.hasNext && !pi.next.isInstanceOf[List[_]]) bldr append pi.next
      }

      bldr.result.replace("_", "")
    }
  }

  implicit class MoreSimClockDomainPimper(cd: ClockDomain) {
    // TODO: more

  }

  val XilinxClockConfig = ClockDomainConfig(resetKind = BOOT)

  type HardInt = BitVector with Num[_ >: SInt with UInt <: BitVector with Num[_ >: SInt with UInt] with MinMaxProvider with DataPrimitives[_ >: SInt with UInt] with BitwiseOp[_ >: SInt with UInt]] with MinMaxProvider with DataPrimitives[_ >: SInt with UInt <: BitVector with Num[_ >: SInt with UInt] with MinMaxProvider with DataPrimitives[_ >: SInt with UInt] with BitwiseOp[_ >: SInt with UInt]] with BitwiseOp[_ >: SInt with UInt <: BitVector with Num[_ >: SInt with UInt] with MinMaxProvider with DataPrimitives[_ >: SInt with UInt] with BitwiseOp[_ >: SInt with UInt]]

  def value2C(number: BigInt, width: Int) = {
    if (width == 1) number
    else {
      val binary = number.toString(2).padToLeft(width, '0')
      -binary.head.asDigit * (BigInt(1) << number.bitLength - 1) + BigInt(binary.tail, 2)
    }
  }

  // some methods for using BigInt as string of bits
  implicit class BigIntUtil(bigInt: BigInt) {
    def toBinary = bigInt.toString(2)
  }

  implicit class BitStringUtil(string: String) {
    def toBigIntAsBinary = BigInt(string)
  }

  implicit class SFixUtil(sf:SFix){
    def unary_-() = {
      val ret = SFix(sf.maxExp exp, sf.minExp exp)
      ret.raw := -sf.raw
      ret
    }
  }
}

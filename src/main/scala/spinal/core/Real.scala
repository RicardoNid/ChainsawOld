package spinal.core

import Chainsaw._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, ceil, log, max, min, pow}

/** Factories in this trait would be automatically visiable in the project, as the package object extends it
 *
 */
trait RealFactory {
  /** Native factory
   */
  def Real(realInfo: RealInfo, resolution: ExpNumber): Real =
    new Real(realInfo, resolution)

  def Real(lower: Double, upper: Double, resolution: ExpNumber): Real =
    Real(RealInfo(lower, upper), resolution)

  def Real(lower: Double, upper: Double, decimalResolution: Double): Real =
    Real(lower, upper, -log2Up(ceil(1 / decimalResolution).toInt) exp)

  def RealWithError(lower: Double, upper: Double, resolution: ExpNumber)(implicit error: Double): Real =
    Real(RealInfo(lower, upper, error), resolution)

  def UIntReal(upper: Int): Real = Real(0.0, upper, 0 exp)

  def SIntReal(upper: Int): Real = Real(-upper, upper, 0 exp)

  def SIntReal(lower: Int, upper: Int): Real = {
    println(s"SIntReal is unnecessary as lower = $lower")
    Real(lower, upper, 0 exp)
  }

  //  https://en.wikipedia.org/wiki/Q_(number_format)
  def QFormatReal(qFormat: QFormat): Real = {
    import qFormat._
    val lower = if (signed) -pow(2, nonFraction - 1) else 0.0
    val upper = if (signed) pow(2, nonFraction - 1) else pow(2, nonFraction)
    new Real(RealInfo(lower, upper), -qFormat.fraction exp)
  }

  /** Constant factory
   */
  def ConstantReal(value: Double, resolution: ExpNumber): Real = {
    val ret = Real(RealInfo(value), resolution)
    println(s"initial: ${ret.realInfo}")
    ret := value
    println(s"after assignment: ${ret.realInfo}")
    ret
  }

  def RealWithError(value: Double, resolution: ExpNumber): Real = {
    val error = abs(value - value.roundAsScala(pow(2.0, resolution.value)))
    val ret = Real(RealInfo(value, error), resolution)
    ret := value
    ret
  }
}

/**
 * @param realInfo all the numeric information of the signal(interval and error), this is a var, so it can be reassigned
 * @param resolution
 * @note The MSB strategy of real is independent of its LSB strategy, more specifically
 *
 *       the MSBs are determined by the intervals
 *
 *       the LSBs are determinde by the resolutions
 *
 *       firstly, they are introduced by the user
 *
 *       then, they are generated through numeric operation and assignments
 *
 *       the basic resolution generating strategy is based on '''significant figures''', which does not allow to lose any of them
 *
 *       a slightly more complex strategy is "lower bound", which drops the bits after a specific resolution
 *
 *       the most complex strategy is
 *
 */
class Real(inputRealInfo: RealInfo, val resolution: ExpNumber, withError: Boolean = false) extends MultiData {

  // TODO: mix signed and unsigned in this type
  // numeric methods for word length determination
  def log2Up(value: Double) = ceil(log2(value)).toInt

  def log2(value: Double) = {
    require(value >= 0.0, s"operand of log2 should >= 0.0, fix it")
    if (value == 0.0) 0.0 else log(value) / log(2.0)
  }

  implicit class realInfo2Bits(info: RealInfo) {

    /** The range is of the format [a, b]
     *
     * @note Considering the following situations where
     *
     *       a, b < 0
     *
     *       a < 0 && b > 0
     *
     *       a, b > 0
     *
     *       theoretically, the best situation is that the resolution/LSB has nothing to do with the MSB
     *
     *       however, as 2's complement is asymmetric, without involving ulp, our estimation for MSB would be very pessimistic
     * @example for upper = 1.9, without knowledge on ulp, only 2 bits(rather than 1) would be safe to include it, as max value of 1Q2 is 1.75
     */
    def getMaxExp(implicit ulp: Double) = {
      def bitsForBound(bound: Double) = if (bound >= 0.0) log2Up(bound + ulp) else log2Up(-bound)

      max(bitsForBound(info.upper), bitsForBound(info.lower))
    }
  }

  // resolution is determined by the argument at the very begining
  val minExp = resolution.value
  implicit val ulp = pow(2, minExp)

  val lower = inputRealInfo.lower.roundDown
  val upper = inputRealInfo.upper.roundUp
  val error = if (withError) ulp else 0.0
  var realInfo = RealInfo(lower, upper, error)

  // determine the inner representation
  val maxExp = inputRealInfo.getMaxExp
  // TODO: pleases notice that this is for signed number only
  val bitCount = maxExp - minExp + 1
  val raw = SInt(bitCount bits)

  // attributes determined after maxExp
  def maxValue: BigDecimal = raw.maxValue.toDouble * ulp
  def minValue: BigDecimal = raw.minValue.toDouble * ulp

  // TODO: delete after verification
  // assertions
  assert(maxExp > minExp,
    s"minExp $minExp >= maxExp $maxExp, " +
      s"as you try to represent $realInfo with a resolution of $resolution, " +
      s"please try to fix it")

  assert(realInfo.lower >= minValue && realInfo.upper <= maxValue,
    s"part of the interval is not presentable, " +
      s"infos: minExp $minExp, maxExp $maxExp, $realInfo, representable [$minValue, $maxValue]")

  // copy from SFix template
  // TODO: figure them out
  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)
  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer("" -> raw)

  // alignment
  def difLsb(that: Real) = this.minExp - that.minExp
  def alignLsb(that: Real): (SInt, SInt) = {
    val lsbDif = difLsb(that)
    val left: SInt = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: SInt = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

  /** Operations
   *
   * @note  operations on Real are defined in the follwing aspects
   *
   *        1. LSB strategy, so the resolution should be determined at the very beginning
   *
   *        2. MSB strategy
   *
   *        3. error introduction and propagation
   *
   *        4. implementation
   *
   */
  def unary_-() = new Real(-realInfo, minExp exp)

  def doAddSub(that: Real, add: Boolean): Real = {
    val (rawLeft, rawRight) = alignLsb(that)
    val realInfo = if (add) this.realInfo + that.realInfo else this.realInfo - that.realInfo
    val ret = new Real(realInfo, min(this.minExp, that.minExp) exp)
    val maxExpEnlarged = realInfo.getMaxExp > max(this.maxExp, that.maxExp)
    val retRaw =
      if (maxExpEnlarged) if (add) rawLeft +^ rawRight else rawLeft -^ rawRight
      else if (add) rawLeft + rawRight else rawLeft - rawRight
    ret.raw := retRaw
    ret
  }

  def +(that: Real) = doAddSub(that, true)

  def -(that: Real) = doAddSub(that, true)

  def *(that: Real): Real = {
    val realInfo = this.realInfo * that.realInfo
    val ret = new Real(realInfo, this.minExp + that.minExp exp)
    val retRaw = this.raw * that.raw
    ret.raw := retRaw.resized
    ret
  }

  /** Arithmetic operation with constant
   *
   * @param thatConstant the constant share the same resolution as the signal, for customized resolution, use Real() or RealWithError() to declare
   */
  def doAddSub(thatConstant: Double, add: Boolean): Real = {
    if (thatConstant.abs <= ulp) {
      println(s"the constant $thatConstant is absorbed as signal $name has ulp = $ulp")
      val ret = this.clone
      ret := this
      ret
    }
    else {
      val that = ConstantReal(thatConstant, this.resolution)
      println(s"operands: ${this.realInfo}, ${that.realInfo}")
      println(doAddSub(that, add).realInfo)
      doAddSub(that, add)
    }
  }

  def +(thatConstant: Double): Real = doAddSub(thatConstant, true)

  def -(thatConstant: Double): Real = doAddSub(thatConstant, false)

  def *(thatConstant: Double): Real = {
    val that = ConstantReal(thatConstant, this.resolution)
    *(that)
  }

  def <<(shiftConstant: Int): Real = {
    val ret = new Real(realInfo << shiftConstant, minExp + shiftConstant exp)
    ret.raw := this.raw
    ret
  }

  def >>(shiftConstant: Int): Real = {
    val ret = new Real(realInfo >> shiftConstant, minExp - shiftConstant exp)
    ret.raw := this.raw
    ret
  }

  // comparsions
  def <(that: Real): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft < rawRight
  }

  def >(that: Real) = that < this

  def <=(that: Real): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft <= rawRight
  }

  def >=(that: Real) = that <= this


  // assignments, all based on BigDecimal method
  def :=(that: BigDecimal): Unit = {
    assert(that <= this.maxValue, s"Literal $that is to big to be assigned in $this")
    assert(that >= this.minValue, s"Literal $that is to negative to be assigned in this $this")
    val intValue = that.toDouble.roundAsScalaInt(ulp)
    this.raw := intValue
  }

  def :=(that: Float): Unit = this := BigDecimal(that.toDouble)

  def :=(that: Double): Unit = this := BigDecimal(that)

  def :=(that: BigInt): Unit = this := BigDecimal(that)

  def :=(that: Int): Unit = this := BigInt(that)

  def :=(that: Long): Unit = this := BigInt(that)

  override private[spinal] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case that if this.getClass.isAssignableFrom(that.getClass) =>
        val t = that.asInstanceOf[Real]
        if (this.maxExp < t.maxExp) { // overflow is not allowed
          val trace = ScalaLocated.long
          globalData.pendingErrors += (() => s"$this can't be assigned by $t because of overflow. please redesign.\n $trace")
        }
        if (this.minExp > t.minExp) { // underflow is allowed when tagTruncated is on
          if (!t.hasTag(tagTruncated)) {
            val trace = ScalaLocated.long
            globalData.pendingErrors += (() => s"$this can't be assigned by $t because of truncation. You can do x := y.truncated if that's fine.\n $trace")
          }
        }
        val difLsb = this.difLsb(t)
        this.realInfo = t.realInfo.clone
        if (difLsb > 0) {
          this.raw compositAssignFrom((t.raw >> difLsb).resized, this.raw, kind)
          this.realInfo = this.realInfo.withErrorAdded(pow(2, this.minExp))
        }
        else if (difLsb < 0)
          this.raw compositAssignFrom((t.raw << -difLsb).resized, this.raw, kind)
        else
          this.raw compositAssignFrom(t.raw.resized, this.raw, kind)
      case _ => SpinalError("Undefined assignment")
    }
  }

  override def autoConnect(that: Data): Unit = autoConnectBaseImpl(that)

  // clone related
  override def clone: Real = new Real(realInfo.clone, resolution)

  def init(that: BigDecimal): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }

  def truncated: this.type = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }
}
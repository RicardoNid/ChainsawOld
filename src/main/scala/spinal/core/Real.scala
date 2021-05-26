package spinal.core

import Chainsaw._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, ceil, log, max, min, pow}

/**
 * @param inputRealInfo     all the numeric information of the signal(interval and error), this is a var, so it can be reassigned, determines MSB
 * @param resolution        the number of bits used for fractional part, determines LSB
 * @param withRoundingError if enabled, the signal will be initialized with rounding error, which is determined by the resolution
 * @note The MSB strategy of real is independent of its LSB strategy, more specifically
 *
 *       the MSBs are determined by the intervals
 *
 *       the LSBs are determined by the resolutions
 *
 *       firstly, they are introduced by the user
 *
 *       then, they are generated through numeric operation and assignments
 *
 *       the basic resolution generating strategy is based on '''significant figures''', which does not allow to lose any of them
 *
 *       a slightly more complex strategy is "lower bound", which drops the bits after a specific resolution that the user specified
 *
 *       the most complex strategy is
 *
 */
class Real(var realInfo: RealInfo, val qWidths: QWidths) extends MultiData {

  // TODO:
  override def clone: Real = new Real(realInfo.clone(), qWidths.copy())

  // attribute accessors
  def error = realInfo.error
  def upper = realInfo.upper
  def lower = realInfo.lower
  def maxExp = qWidths.maxExp
  def minExp = qWidths.minExp
  def resolution = qWidths.minExp exp
  def ulp: Double = pow(2, minExp)

  // implement raw
  val bitCount = maxExp - minExp + 1
  val raw = SInt(bitCount bits)
  def maxValue: BigDecimal = raw.maxValue.toDouble * ulp
  def minValue: BigDecimal = raw.minValue.toDouble * ulp
  def isUnsigned = lower * upper >= 0.0
  def isPositive = lower >= 0.0
  // TODO: fully implement this
  def isNegative = upper <= 0.0

  // validators & warnings
  // any minExp, no matter generated or declared, should be constrained by ChainsawExpLowerBound
  assert(minExp >= ChainsawExpLowerBound, s"minExp $minExp is lower than the lower bound $ChainsawExpLowerBound ")

  // interval relationship: representable interval should cover the real interval
  assert(lower >= minValue && upper <= maxValue,
    s"representable interval [$minValue, $maxValue] doesn't cover, " +
      s"the real interval $realInfo " +
      s"as the QWidths is $qWidths")

  if (maxExp <= minExp) printlnYellow(
    s"you try to pass a value within [$lower, $upper], but the ulp is $ulp, " +
      s"as a result, it became zero")

  /** Error is generally a tag, so we implement it in a var style
   */
  def withRoundingError = {
    this.realInfo.error += ulp
    this
  }

  // TODO: figure these out(copied from SFix)
  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)
  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer("" -> raw)

  // implementation of operations
  def difLsb(that: Real) = this.minExp - that.minExp

  def alignLsb(that: Real): (SInt, SInt) = {
    val lsbDif = difLsb(that)
    val left: SInt = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: SInt = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

  def alignLsbAndDivide(that: Real) = {
    val lowerCount = difLsb(that).abs
    val highLeft: SInt = if (difLsb(that) > 0) this.raw else this.raw(this.raw.getBitsWidth - 1 downto lowerCount)
    val highRight: SInt = if (difLsb(that) > 0) that.raw(that.raw.getBitsWidth - 1 downto lowerCount) else that.raw
    val lower: Bits =
      (if (difLsb(that) > 0) that.raw(lowerCount - 1 downto 0)
      else this.raw(lowerCount - 1 downto 0)).asBits
    (highLeft, highRight, lower)
  }

  /** Operations
   *
   * @note operations on Real are defined in the following aspects
   *
   *       1.LSB strategy, so the resolution should be determined at the very beginning
   *
   *       2.MSB strategy
   *
   *       3.error introduction and propagation
   *
   *       4.implementation
   *
   */
  def unary_-() = {
    val ret = Real(-realInfo, minExp exp)
    println(this)
    println(ret)
    ret.raw := -this.raw
    ret
  }

  def doAddSub(that: Real, add: Boolean): Real = {
    // numeric inference
    printlnWhenNumericDebug(s"before addition, a: $this, b: $that")
    // LSB strategy, lower bound rule check is skipped as this.minExp and that.minExp have been checked earlier
    val minExp = min(this.minExp, that.minExp)
    // MSB & error strategy
    val realInfo = if (add) this.realInfo + that.realInfo else this.realInfo - that.realInfo

    val ret = Real(realInfo, minExp exp)
    printlnWhenNumericDebug(s"after addition, a + b: $ret")

    val maxExpEnlarged = ret.maxExp > max(this.maxExp, that.maxExp)
    val maxExpReduced = ret.maxExp < max(this.maxExp, that.maxExp)
    printlnWhenNumericDebug(s"MSB Enlarged: $maxExpEnlarged, MSB Reduced: $maxExpReduced")

    // implementation
    // HDL implementation
    val (rawLeft, rawRight) = alignLsb(that)
    // TODO: fully implement this
    if (this.isUnsigned && that.isUnsigned) printlnWhenNumericDebug(s"do unsigned arithmetic")
    val retRaw =
      if (this.isPositive && that.isPositive) {
        if (maxExpEnlarged) if (add) rawLeft.asUInt +^ rawRight.asUInt else rawLeft.asUInt -^ rawRight.asUInt
        else if (add) rawLeft.asUInt + rawRight.asUInt else rawLeft.asUInt - rawRight.asUInt
      }.asSInt
      else {
        if (maxExpEnlarged) if (add) rawLeft +^ rawRight else rawLeft -^ rawRight
        else if (add) rawLeft + rawRight else rawLeft - rawRight
      }

    // post alignment
    if (maxExpReduced) ret.raw := retRaw.resized
    else ret.raw := retRaw
    ret
  }

  def +(that: Real) = doAddSub(that, add = true)

  def -(that: Real) = doAddSub(that, add = false)

  def *(that: Real): Real = {
    // numeric inference
    printlnWhenNumericDebug(s"before multiplication, a: $this, b: $that")
    // LSB strategy, lower bound rule should be checked
    val minExpByVerilog = this.minExp + that.minExp
    val bitsTruncated = ChainsawExpLowerBound - minExpByVerilog
    val minExpReduced = bitsTruncated > 0
    val minExp = if (minExpReduced) ChainsawExpLowerBound else minExpByVerilog
    val maxExpByVerilog = this.maxExp + that.maxExp + 1

    // MSB & error strategy
    val realInfo = this.realInfo * that.realInfo

    val ret = Real(realInfo, minExp exp)

    val maxExpEnlarged = ret.maxExp > maxExpByVerilog
    assert(!maxExpEnlarged, "maxExpEnlarged should always be false as Chainsaw result is more accurate than verilog")
    val maxExpReduced = ret.maxExp < maxExpByVerilog
    printlnWhenNumericDebug(s"MSB Reduced: $maxExpReduced")

    // side effect of LSB strategy
    if (minExpReduced) ret.withRoundingError
    printlnWhenNumericDebug(s"after multiplication, a * b: $ret")

    // implementation
    // HDL implementation
    val retRaw =
    // TODO: figure out the unsigned arithmetic
    if (this.isPositive && that.isPositive) this.raw * that.raw
    else this.raw * that.raw

    // post alignment
    val minExpAligned = if (minExpReduced) retRaw(retRaw.getBitsWidth - 1 downto bitsTruncated) else retRaw
    ret.raw := (if (maxExpReduced) minExpAligned.resized else minExpAligned)
    ret
  }

  /** Operation with constant
   *
   * @param thatConstant the constant which share the same resolution as the signal,
   *
   *                     for customized resolutions, use RealConstant() or RealConstantWithError() to declare them first
   */
  def doAddSub(thatConstant: Double, add: Boolean): Real = {
    if (thatConstant.abs <= ulp) {
      println(s"the constant $thatConstant is absorbed through addition/subtraction as signal $name has ulp = $ulp")
      // implementation
      val ret = this.clone
      ret := this
      ret
    }
    else {
      val that = R(thatConstant, this.resolution)
      doAddSub(that, add)
    }
  }

  def +(thatConstant: Double): Real = doAddSub(thatConstant, add = true)

  def -(thatConstant: Double): Real = doAddSub(thatConstant, add = false)

  def *(thatConstant: Double): Real = {
    println(s"try to multiply $thatConstant")
    val that = R(thatConstant, this.resolution)
    printlnGreen(s"auto-implemented constant $thatConstant, $that")
    *(that)
  }

  /** << and >>, they won't introduce new error/significant figures
   */
  def <<(shiftConstant: Int): Real = {
    val minExp = this.minExp + shiftConstant // LSB strategy
    val realInfo = this.realInfo << shiftConstant // MSB strategy
    val ret = Real(realInfo, minExp exp)
    ret.raw := this.raw
    ret
  }

  def >>(shiftConstant: Int): Real = {
    val minExpByVerilog = this.minExp - shiftConstant
    val truncated = ChainsawExpLowerBound - minExpByVerilog
    val minExp = if (truncated > 0) ChainsawExpLowerBound else minExpByVerilog // LSB strategy, no rounding error introduced
    val realInfo = this.realInfo >> shiftConstant // MSB strategy
    val ret = Real(realInfo, minExp exp)
    ret.raw :=
      (if (truncated > 0) this.raw(this.raw.getBitsWidth - 1 downto truncated).resized
      else this.raw)
    ret
  }


  /** Comparisons
   */
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


  /** Assignments from constants, the BigDecimal version is the base of other versions
   */
  def :=(thatConstant: BigDecimal): Unit = {
    assert(thatConstant <= this.maxValue, s"Literal $thatConstant is to big to be assigned in $this")
    assert(thatConstant >= this.minValue, s"Literal $thatConstant is to negative to be assigned in this $this")
    val intValue = thatConstant.toDouble.roundAsScalaInt(ulp)
    this.raw := intValue
  }

  def :=(thatConstant: Float): Unit = this := BigDecimal(thatConstant)

  def :=(thatConstant: Double): Unit = this := BigDecimal(thatConstant)

  def :=(thatConstant: BigInt): Unit = this := BigDecimal(thatConstant)

  def :=(thatConstant: Int): Unit = this := BigInt(thatConstant)

  def :=(thatConstant: Long): Unit = this := BigInt(thatConstant)

  /** Assignment from signal, which is implemented by overriding assignFromImpl
   */
  override private[spinal] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    that match {
      case that if this.getClass.isAssignableFrom(that.getClass) =>
        val t = that.asInstanceOf[Real]
        if (this.maxExp < t.maxExp) { // overflow is not allowed
          if (!t.hasTag(tagTruncated)) {
            val trace = ScalaLocated.long
            globalData.pendingErrors += (() => s"$this can't be assigned by \n" +
              s"$t because of overflow. please redesign.\n " +
              s"$trace")
          }
          else println(s"$this is being assigned by $t which may leads overflow, make sure that it is fine")
        }
        if (this.minExp > t.minExp) { // underflow is allowed when tagTruncated is on
          if (!t.hasTag(tagTruncated)) {
            val trace = ScalaLocated.long
            globalData.pendingErrors += (() => s"$this can't be assigned by $t because of truncation. You can do x := y.truncated if that's fine.\n $trace")
          }
        }
        val difLsb = this.difLsb(t)
        this.realInfo = t.realInfo.clone
        if (difLsb > 0) { // that is, when this has fewer fractional bits than that
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

  def init(that: BigDecimal): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init initValue
    this
  }

  def truncated: this.type = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }

  override def toString() = s"$name minExp $minExp, maxExp $maxExp, $realInfo, representable [$minValue, $maxValue]"
}
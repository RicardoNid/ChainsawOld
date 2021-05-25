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
class Real(inputRealInfo: RealInfo, val resolution: ExpNumber, withRoundingError: Boolean = false) extends MultiData {

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

  override def clone: Real = new Real(realInfo.clone, resolution)

  // resolution is determined by the argument at the very beginning
  val minExp = resolution.value
  assert(minExp >= ChainsawExpLowerBound)
  implicit val ulp: Double = pow(2, minExp)

  // realInfo refinement by rounding error

  // TODO: no it will not exceed?

  /** The reason of adjusting realInfo is that,
   * because of the rounding error, the interval of realInfo may exceed the actual representable interval,
   * when this accumulates, a signal may have a insufficient width for its interval
   */
  //  def adjustingRealInfoInterval(realInfo: RealInfo): Unit = {
  //    val upper = inputRealInfo.upper.roundUp
  //    val lower = inputRealInfo.lower.roundDown
  //    RealInfo(lower, upper, error err)
  //  }

  val propagatedError = inputRealInfo.error
  val roundingError =
    if (inputRealInfo.isConstant) abs(inputRealInfo.constant - inputRealInfo.constant.roundAsScala) // when initialized by a constant
    else ulp
  var error = if (withRoundingError) propagatedError + roundingError else propagatedError

  var realInfo = new RealInfo(inputRealInfo.interval.clone, error)
  val upper = realInfo.upper
  val lower = realInfo.lower

  // determine the inner representation
  val maxExp = inputRealInfo.getMaxExp
  // TODO: pleases notice that this is for signed number only
  val bitCount = maxExp - minExp + 1
  val raw = SInt(bitCount bits)

  // constraints
  // TODO: improve this
  //  assert(maxExp <= 24 && minExp >= -25 && maxExp >= minExp,
  //    "currently, we do not allow exponents outside of [-25, 24], " +
  //      "because of the limitation of double-precision backend")

  // attributes determined after maxExp
  def maxValue: BigDecimal = raw.maxValue.toDouble * ulp

  def minValue: BigDecimal = raw.minValue.toDouble * ulp

  // TODO: delete after verification
  // assertions
  if (maxExp <= minExp) printlnYellow(s"minExp $minExp >= maxExp $maxExp, " +
    s"as you try to represent $realInfo with a resolution of $resolution, " +
    s"so this becomes zero and absorbed")
  //  assert(maxExp > minExp,
  //    s"minExp $minExp >= maxExp $maxExp, " +
  //      s"as you try to represent $realInfo with a resolution of $resolution, " +
  //      s"please try to fix it")

  assert(realInfo.lower >= minValue && realInfo.upper <= maxValue,
    s"part of the interval is not presentable, " +
      s"infos: minExp $minExp, maxExp $maxExp, $realInfo, representable [$minValue, $maxValue]")


  // copy from SFix template
  // TODO: figure these out
  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer("" -> raw)

  // alignment of raws
  def difLsb(that: Real) = this.minExp - that.minExp

  def alignLsb(that: Real): (SInt, SInt) = {
    val lsbDif = difLsb(that)
    val left: SInt = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: SInt = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

  def alignLsbAndDivide(that: Real) = {
    val lowerCount = difLsb(that).abs
    //    val (left, right) = alignLsb(that)
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
    val ret = new Real(-realInfo, minExp exp)
    println(this)
    println(ret)
    ret.raw := -this.raw
    ret
  }

  def doAddSub(that: Real, add: Boolean): Real = {
    val minExp = min(this.minExp, that.minExp) // LSB strategy, no rounding error introduced
    val realInfo = if (add) this.realInfo + that.realInfo else this.realInfo - that.realInfo // MSB strategy
    println(s"when it comes to Real ${realInfo.interval.intervalTerms.keySet.mkString(" ")}")
    val ret = new Real(realInfo, minExp exp)
    println(s"when it comes to Real later ${ret.realInfo.interval.intervalTerms.keySet.mkString(" ")}")

    val maxExpEnlarged = ret.maxExp > max(this.maxExp, that.maxExp)
    val maxExpReduced = ret.maxExp < max(this.maxExp, that.maxExp)
    println(s"maxEnlarged: $maxExpEnlarged")
    println(s"maxReduced: $maxExpReduced")

    // implementation
    val (rawLeft, rawRight) = alignLsb(that)
    val retRaw = // TODO: improve the performance of this part
      if (this.lower >= 0 && that.lower >= 0) {
        println("do uint arith")
        if (maxExpEnlarged) if (add) rawLeft.asUInt +^ rawRight.asUInt else rawLeft.asUInt -^ rawRight.asUInt
        else if (add) rawLeft.asUInt + rawRight.asUInt else rawLeft.asUInt - rawRight.asUInt
      }.asSInt
      else {
        if (maxExpEnlarged) if (add) rawLeft +^ rawRight else rawLeft -^ rawRight
        else if (add) rawLeft + rawRight else rawLeft - rawRight
      }



    // TODO: fix sub
    //    val (highLeft, highRight, lower) = alignLsbAndDivide(that)
    //    val retRaw =
    //      (if (maxExpEnlarged) if (add) (highLeft +^ highRight) ## lower else (highLeft -^ highRight) ## lower
    //      else if (add) (highLeft + highRight) ## lower else (highLeft - highRight) ## lower).asSInt

    if (maxExpReduced) ret.raw := retRaw.resized // sometimes, because of cancellation, the interval may be narrower
    else ret.raw := retRaw
    ret
  }

  def +(that: Real) = doAddSub(that, add = true)

  def -(that: Real) = doAddSub(that, add = false)

  def *(that: Real): Real = {

    val minExpByVerilog = this.minExp + that.minExp
    val truncated = ChainsawExpLowerBound - minExpByVerilog
    val minExp = if (truncated > 0) ChainsawExpLowerBound else minExpByVerilog // LSB strategy, no rounding error introduced

    val realInfo = this.realInfo * that.realInfo // MSB strategy
    printlnGreen(s"multiplication this: ${this.realInfo}, that: ${that.realInfo}, result: $realInfo")
    val ret = new Real(realInfo, minExp exp)
    // implementation
    // TODO: consider all the effects of user-defined lowerBound
    val retRaw = this.raw * that.raw
    ret.raw :=
      (if (truncated > 0) retRaw(retRaw.getBitsWidth - 1 downto truncated).resized
      else retRaw.resized)
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
      val that = ConstantRealWithError(thatConstant, this.resolution)
      doAddSub(that, add)
    }
  }

  def +(thatConstant: Double): Real = doAddSub(thatConstant, add = true)

  def -(thatConstant: Double): Real = doAddSub(thatConstant, add = false)

  def *(thatConstant: Double): Real = {
    println(s"try to multiply $thatConstant")
    val that = ConstantRealWithError(thatConstant, this.resolution)
    printlnGreen(s"auto-implemented constant $thatConstant, $that")
    *(that)
  }

  /** << and >>, they won't introduce new error/significant figures
   */
  def <<(shiftConstant: Int): Real = {
    val minExp = this.minExp + shiftConstant // LSB strategy
    val realInfo = this.realInfo << shiftConstant // MSB strategy
    val ret = new Real(realInfo, minExp exp)
    ret.raw := this.raw
    ret
  }

  def >>(shiftConstant: Int): Real = {
    val minExpByVerilog = this.minExp - shiftConstant
    val truncated = ChainsawExpLowerBound - minExpByVerilog
    val minExp = if (truncated > 0) ChainsawExpLowerBound else minExpByVerilog // LSB strategy, no rounding error introduced
    val realInfo = this.realInfo >> shiftConstant // MSB strategy
    val ret = new Real(realInfo, minExp exp)
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
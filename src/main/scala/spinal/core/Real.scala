package spinal.core

import Chainsaw._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, ceil, floor, log, max, min, pow}

trait RealFactory {
  /** Native factory
   */
  def Real(realInfo: RealInfo, resolution: ExpNumber) =
    new Real(realInfo, resolution)

  def Real(lower: Double, upper: Double, resolution: ExpNumber) =
    new Real(RealInfo(lower, upper), resolution)

  def Real(lower: Double, upper: Double, decimalResolution: Double) =
    new Real(RealInfo(lower, upper), -log2Up(ceil(1 / decimalResolution).toInt) exp)

  def RealWithError(lower: Double, upper: Double, resolution: ExpNumber)(implicit error: Double) =
    new Real(RealInfo(lower, upper)(error), resolution)

  /**
   */
  def UIntReal(upper: Int) = new Real(RealInfo(0.0, upper), 0 exp)

  /**
   */
  def SIntReal(upper: Int) = new Real(RealInfo(-upper, upper), 0 exp)

  def SIntReal(lower: Int, upper: Int) = {
    println(s"SIntReal is unnecessary as lower = $lower")
    new Real(RealInfo(lower, upper), 0 exp)
  }

  //  https://en.wikipedia.org/wiki/Q_(number_format)
  def QFormatReal(qFormat: QFormat) = {
    import qFormat._
    val lower = if (signed) -pow(2, nonFraction - 1) else 0.0
    val upper = if (signed) pow(2, nonFraction - 1) else pow(2, nonFraction)
    new Real(RealInfo(lower, upper), -qFormat.fraction exp)
  }

  def Real(value: Double, resolution: ExpNumber) = {
    val ret = new Real(RealInfo(value), resolution)
    println(s"initial: ${ret.realInfo}")
    ret := value
    println(s"after assignment: ${ret.realInfo}")
    ret
  }

  def RealWithError(value: Double, resolution: ExpNumber) = {
    val error = abs(value - value.roundAsScala(pow(2.0, resolution.value)))
    val ret = new Real(RealInfo(value, error), resolution)
    ret := value
    ret
  }
}

class Real(var realInfo: RealInfo, val resolution: ExpNumber) extends MultiData {

  def log2Up(value: Double) = ceil(log2(value)).toInt

  def log2(value: Double) = {
    require(value >= 0.0)
    // situation 0.0 is contained here
    // TODO: a better solution
    if (value == 0.0) 0.0 else log(value) / log(2.0)
  }

  implicit class realInfo2Bits(info: RealInfo) {

    /** The range is of the format [a, b]
     *
     */
    def getMaxExp = {
      val upperBits = if (info.upper > 0) floor(log2(info.upper) + 1).toInt else log2Up(info.upper.abs)
      max(upperBits, log2Up(info.lower.abs))
    }
  }

  // expose the numeric infos
  val lower = realInfo.lower
  val upper = realInfo.upper
  val error = realInfo.error

  val minExp = resolution.value
  val ulp = pow(2, minExp)
  val maxExp = realInfo.getMaxExp
  val bitCount = maxExp - minExp + 1

  def maxValue: BigDecimal = pow(2, maxExp) - ulp

  def minValue: BigDecimal = -pow(2, maxExp)

  val raw = SInt(bitCount bits)

  // TODO: delete after verification
  assert(ceil(lower / ulp).toInt >= raw.minValue, s"insufficient width at $name, " +
    s"maxExp = $maxExp, minExp = $minExp, range = ${realInfo}, lower = $lower, representable = ${raw.minValue.toDouble * ulp}, ulp = $ulp")
  //  val upperRepresentable = if (floor(upper / ulp).toInt > 0) floor(upper / ulp).toInt - 1 else floor(upper / ulp).toInt
  assert(floor(upper / ulp).toInt <= raw.maxValue, s"insufficient width at $name, " +
    s"maxExp = $maxExp, minExp = $minExp, range = ${realInfo}, upper = $upper, representable = ${raw.maxValue.toDouble * ulp}, ulp = $ulp")

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  def difLsb(that: Real) = this.minExp - that.minExp

  def alignLsb(that: Real): (SInt, SInt) = {
    val lsbDif = difLsb(that)
    val left: SInt = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: SInt = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

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
      val that = Real(thatConstant, this.resolution)
      println(s"operands: ${this.realInfo}, ${that.realInfo}")
      println(doAddSub(that, add).realInfo)
      doAddSub(that, add)
    }
  }

  def +(thatConstant: Double): Real = doAddSub(thatConstant, true)

  def -(thatConstant: Double): Real = doAddSub(thatConstant, false)

  def *(thatConstant: Double): Real = {
    val that = Real(thatConstant, this.resolution)
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
          this.realInfo = this.realInfo.errorAdded(pow(2, this.minExp))
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
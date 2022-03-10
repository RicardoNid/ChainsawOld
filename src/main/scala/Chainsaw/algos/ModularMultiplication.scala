package Chainsaw.algos

import Chainsaw.{definition, hardAlgo}
import breeze.numerics.ceil

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** modular multiplication algorithms, they validate themselves while running
 *
 */
object ModularMultiplication {

  import spinal.core._
  import spinal.core.sim._

  // modular multiplication test util
  def evaluateMM(X: BigInt, Y: BigInt, n: Int, hardware: (UInt, UInt) => UInt): BigInt = {
    var ret = BigInt(0)
    SimConfig.withWave.compile(new Component {
      val x = in UInt (n bits)
      val y = in UInt (n bits)
      val ret = out UInt (n bits)
      ret := hardware(x, y)
    }).doSim { dut =>
      dut.x #= X
      dut.y #= Y
      sleep(1)
      ret = dut.ret.toBigInt
    }
    ret
  }

  /** Montgomery modular multiplication
   *
   * @param N modulo
   * @return xyR^-1^ mod N
   * @see ''Modular multiplication without trial division'' [[https://www.ams.org/mcom/1985-44-170/S0025-5718-1985-0777282-X/S0025-5718-1985-0777282-X.pdf]]
   */
  @definition
  def mmm(x: BigInt, y: BigInt, N: BigInt): BigInt = {

    // use number length of current crypto system
    //    val lN = nextPower2(N.bitLength).toInt
    // or, use the nearest power of 2
    val lN = N.bitLength

    // preparing parameters
    // TODO: a propre R for RSA?
    val R = BigInt(1) << lN
    // TODO: algo to get RPrime & NPrime
    val RInverse = R.modInverse(N)
    val NPrime = (R * RInverse - 1) / N

    // calculation
    val T = x * y
    val m = ((T % R) * NPrime) % R
    val t = (T + m * N) / R

    val ret = if (t >= N) t - N else t // t \in [0, 2N)
    assert(ret == (x * y * RInverse) % N)
    ret
  }

  /** McLaughlin Montgomery modular multiplication
   *
   * @see ''NEW FRAMEWORKS FOR MONTGOMERY'S MODULAR MULTIPLICATION METHOD'' variation 2 [[https://www.ams.org/journals/mcom/2004-73-246/S0025-5718-03-01543-6/S0025-5718-03-01543-6.pdf]]
   */
  @definition
  def mlm(a: BigInt, b: BigInt, N: BigInt): BigInt = {

    // use number length of current crypto system
    //    val lN = nextPower2(N.bitLength).toInt
    // or, use the nearest power of 2
    val lN = N.bitLength

    // preparing parameters
    val R = (BigInt(1) << lN) - 1
    // TODO: verify that GCD(R, N) == 1
    val QPrime = (BigInt(1) << lN) + 1
    val RInverse = R.modInverse(N)
    val NPrime = (R * RInverse - 1) / N

    // calculation
    val m = (a * b * NPrime) % R
    val S = (a * b + m * N) % QPrime
    val w = -S % QPrime + QPrime // or, scala would generate negative value

    // conditional selection
    val s = if (w % 2 == 0) w / 2 else (w + QPrime) / 2
    val t = if ((s % 2) == ((a * b + m * N) % 2)) s else s + QPrime
    val ret = if (t >= N) t - N else t // t \in [0, 2N)

    assert(ret == (a * b * RInverse) % N)
    ret
  }

  /** Improved McLaughlin Montgomery modular multiplication, avoid conditional selections by setting new (looser) bounds and thus consume more width
   *
   * @param x \in [0, 2N)
   * @param y \in [0, 2N)
   * @return ret = xyR^-1^ (mod N) \in [0, 2N)
   * @see ''FFT-Based McLaughlin’s MontgomeryExponentiation without Conditional Selections'' [[http://cetinkoc.net/docs/j81.pdf]]
   */
  @hardAlgo("mlm")
  def mlws(x: BigInt, y: BigInt, N: BigInt): BigInt = {

    val lN = N.bitLength + 2 // s,t r,h > 4n

    // preparing parameters
    val r = (BigInt(1) << lN) - 1 // corresponding to R in MLM
    // TODO: verify that GCD(R, N) == 1
    val h = (BigInt(1) << lN) + 1 // corresponding to QPrime in MLM
    val RInverse = r.modInverse(N)
    val NPrime = (r * RInverse - 1) / N

    // calculation
    val m = (x * y * NPrime) % r
    val g = (x * y + m * N) % h
    val w = (h - g) / 2 // or, scala would generate negative value

    val ret = w // r \in [0, 2N)
    assert((ret % N) == (x * y * RInverse) % N)
    ret
  }

  /** radix-2 Montgomery multiplication
   *
   * @see ''New Hardware Architectures for Montgomery Modular Multiplication Algorithm'' [[https://ieeexplore.ieee.org/document/5669264]]
   */
  @hardAlgo("mmm")
  def r2mm(X: BigInt, Y: BigInt, M: BigInt) = {
    require(X < M && Y < M)
    val n = M.bitLength // log2Down(M) + 1

    def hardwareCalculator(X: UInt, Y: UInt) = {
      val zero = U(0, n bits)
      val bitsM = U(M, n bits)
      bitsM.setName("M")

      val S = Seq.fill(n + 1)(UInt(n + 1 bits)) // n + 1 bits
      (0 until n + 1).foreach(i => S(i).setName(s"S_$i"))
      S(0) := zero.resized

      def connect(i: Int): Unit = {
        val xi = X(i)
        xi.setName(s"x_$i")
        val qi = (xi & Y.lsb) ^ S(i).lsb // parity of Si + xi * Y
        qi.setName(s"q_$i")
        val yPart = Mux(xi, Y, zero)
        val MPart = Mux(qi, bitsM, zero)
        S(i + 1) := (S(i) +^ (yPart +^ MPart)) >> 1 // n + 1 bits
      }

      (0 until n).foreach(connect)

      val ret = Mux(S.last > bitsM, S.last - bitsM, S.last) // reduction
      ret.resize(n)
    }

    val ret = evaluateMM(X, Y, n, hardwareCalculator)

    val R = BigInt(1) << n
    val RInverse = R.modInverse(M)
    assert((ret % M) == (X * Y * RInverse) % M)
    ret
  }

  /** multi word radix-2 Montgomery modular multiplication, implemented by a hardware calculator
   *
   * @see ''New Hardware Architectures for Montgomery Modular Multiplication Algorithm'' [[https://ieeexplore.ieee.org/document/5669264]]
   */
  @hardAlgo("mmm")
  def mwr2mm(X: BigInt, Y: BigInt, M: BigInt, w: Int) = {
    require(X < M && Y < M)
    val n = M.bitLength // log2Down(M) + 1
    val e = ceil((n + 1) / w.toDouble).toInt
    val dataWidth = e * w

    def hardwareCalculator(X: UInt, Y: UInt): UInt = {
      val zero = U(0, w bits)
      val bitsM = U(M, dataWidth bits)

      val S = Seq.fill(e + 1)(ArrayBuffer[UInt](zero))
      val YWords = Y.subdivideIn(w bits) :+ zero
      val MWords = bitsM.subdivideIn(w bits) :+ zero
      val C = ArrayBuffer[Bits]()

      def combine(xi: Bool, qi: Bool, Y: UInt, M: UInt, S: UInt) = Mux(xi, Y, zero) +^ Mux(qi, M, zero) +^ S

      (0 until n).foreach { i =>
        val xi = X(i)
        val qi = (xi & Y.lsb) ^ S(0).last.lsb
        val (c, s) = combine(xi, qi, YWords(0), MWords(0), S(0).last).splitAt(w)
        S(0) += s.asUInt
        C += c
        (1 to e).foreach { j =>
          val (c, s) = (combine(xi, qi, YWords(j), MWords(j), S(j).last) + C.last.asUInt).splitAt(w)
          S(j) += s.asUInt
          C += c
          S(j - 1) += (S(j).last.lsb ## S(j - 1).last(w - 1 downto 1)).asUInt
        }
        S(e) += zero
      }
      Vec(S.map(_.last)).asBits.asUInt.resize(dataWidth)
    }

    val ret = evaluateMM(X, Y, dataWidth, hardwareCalculator)

    val R = BigInt(1) << n
    val RInverse = R.modInverse(M)
    assert((ret % M) == (X * Y * RInverse) % M)
    ret
  }

  /**
   * @see ''Discrete weighted transforms and large-integer arithmetic'' [[https://www.ams.org/mcom/1994-62-205/S0025-5718-1994-1185244-1/S0025-5718-1994-1185244-1.pdf]]
   */
  def fftms(x: BigInt, y: BigInt, N: BigInt) :BigInt = ???

}

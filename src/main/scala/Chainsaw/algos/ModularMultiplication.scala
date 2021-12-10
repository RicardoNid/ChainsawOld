package Chainsaw.algos

import breeze.linalg.min
import breeze.numerics.ceil
import spinal.core.UInt

import scala.collection.mutable.ArrayBuffer

/** modular multiplication algorithms, they validate themselves while running
 *
 */
object ModularMultiplication {

  import spinal.core._
  import spinal.lib._
  import spinal.core.sim._

  def evaluateMM(X: BigInt, Y: BigInt, M: BigInt, n: Int, hardware: (UInt, UInt) => UInt): BigInt = {
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

  def viewBigInt(x: BigInt) = {
    if (x < 0) println("negative")
    println(s"width: ${x.bitLength}")
  }

  /** Montgomery modular multiplication
   *
   * @param N modulo
   * @return xyR^-1^ mod N
   * @see ''Modular multiplication without trial division'' [[https://www.ams.org/mcom/1985-44-170/S0025-5718-1985-0777282-X/S0025-5718-1985-0777282-X.pdf]]
   */
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
   * @see ''NEW FRAMEWORKSFOR MONTGOMERY'S MODULAR MULTIPLICATIONMETHOD'' variation 2 [[https://www.ams.org/journals/mcom/2004-73-246/S0025-5718-03-01543-6/S0025-5718-03-01543-6.pdf]]
   */
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

      def connect(i: Int) = {
        val xi = X(i)
        xi.setName(s"x_$i")
        val qi = (xi & Y.lsb) ^ S(i).lsb // parity of Si + xi * Y
        qi.setName(s"q_$i")
        val yPart = Mux(xi, Y, zero)
        val MPart = Mux(qi, bitsM, zero)
        S(i + 1) := (S(i) +^ (yPart +^ MPart)) >> 1 // n + 1 bits
      }

      (0 until n).foreach(connect(_))

      val ret = Mux(S.last > bitsM, S.last - bitsM, S.last) // reduction
      ret.resize(n)
    }

    val ret = evaluateMM(X, Y, M, n, hardwareCalculator)

    val R = BigInt(1) << n
    val RInverse = R.modInverse(M)
    assert((ret % M) == (X * Y * RInverse) % M)
    ret
  }

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

    val ret = evaluateMM(X, Y, M, dataWidth, hardwareCalculator)

    val R = BigInt(1) << n
    val RInverse = R.modInverse(M)
    assert((ret % M) == (X * Y * RInverse) % M)
    ret
  }

  def optimizedMwr2mm(X: BigInt, Y: BigInt, M: BigInt, w: Int) = {
    require(X < M && Y < M)
    val n = M.bitLength // log2Down(M) + 1
    val e = ceil((n + 1) / w.toDouble).toInt
    val dataWidth = e * w

    def zero = U(0, w bits)


    case class PEIO() extends Bundle {
      val SLsb = Bool
      val SHigh = UInt(w - 1 bits)
      val YWord = UInt(w bits)
      val MWord = UInt(w bits)
      val start = Bool()
    }

    case class PE() extends Component {
      val pipeIn = in(PEIO())
      val pipeOut = out(PEIO())
      val xi = in Bool()

      import pipeIn._

      val qi = (xi & YWord.lsb) ^ SHigh.lsb
      val qiReg = RegNextWhen(qi, start)
      val qiInUse = Mux(start, qi, qiReg)

      val sEven = (B"0" ## SHigh).asUInt
      val sOdd = (B"1" ## SHigh).asUInt

      val evenReg, oddReg = Reg(UInt(w + 2 bits))
      val trueRet = Mux(SLsb, oddReg, evenReg)
      val (cOut, sOut) = trueRet.splitAt(w)
      val cInUse = Mux(start, U(0, 2 bits), cOut.asUInt)

      def core(xi: Bool, qi: Bool, Y: UInt, M: UInt, S: UInt, C: UInt) = (Mux(xi, Y, zero) +^ Mux(qi, M, zero)) +^ (S +^ C)

      evenReg := core(xi, qiInUse, YWord, MWord, sEven, cInUse) // w + 2 bits
      oddReg := core(xi, qiInUse, YWord, MWord, sOdd, cInUse)

      pipeOut.start := RegNext(start, init = False)
      pipeOut.MWord := RegNext(MWord)
      pipeOut.YWord := RegNext(YWord)
      pipeOut.SHigh := sOut(w - 1 downto 1).asUInt
      pipeOut.SLsb := sOut.lsb
    }

    case class PEArray() extends Component {

      val zero = U(0, w bits)
      val bitsM = U(M, dataWidth bits)
      val X, Y = in UInt (dataWidth bits)
      val ret = out UInt (n bits) // TODO: check range of every step
      val valid = out Bool()

      val YWords = Vec(Y.subdivideIn(w bits) :+ zero)
      val MWords = Vec(bitsM.subdivideIn(w bits) :+ zero)

      val counter = CounterFreeRun(e + 1)
      val pes = Seq.fill(n)(PE())
      X.asBools.take(n).zip(pes).foreach { case (bool, pe) => pe.xi := bool }

      pes.head.pipeIn.SLsb := False
      pes.head.pipeIn.SHigh := U(0, w - 1 bits)
      pes.head.pipeIn.MWord := MWords(counter)
      pes.head.pipeIn.YWord := YWords(counter)
      pes.head.pipeIn.start := (counter === U(0))

      pes.tail.zip(pes.init).foreach { case (next, prev) => next.pipeIn := prev.pipeOut }
      pes.map { pe => pe.pipeOut.SHigh ## pe.pipeOut.SLsb }.asBits().asUInt

      val outCounter = Counter(e + 1)
      when(pes.last.pipeOut.start || outCounter =/= outCounter.getZero)(outCounter.increment())

      val outRegs = Reg(Vec(UInt(w bits), e + 1))
      outRegs(outCounter) := (pes.last.pipeOut.SHigh ## pes.last.pipeOut.SLsb).asUInt
      ret := outRegs.asBits(n downto 1).asUInt
      valid := Delay(pes.last.pipeOut.start, e, init = False)
    }

    var ret = BigInt(0)
    SimConfig.withWave.compile(PEArray()).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.X #= X
      dut.Y #= Y
      dut.clockDomain.waitSampling(n + e + 1)
      ret = dut.ret.toBigInt
    }

    val R = BigInt(1) << n
    val RInverse = R.modInverse(M)
    val golden = (X * Y * RInverse) % M
    println(golden.toString(16))
    assert((ret % M) == (X * Y * RInverse) % M)
    ret
  }

  /**
   * @see ''Discrete weighted transforms and large-integer arithmetic'' [[https://www.ams.org/mcom/1994-62-205/S0025-5718-1994-1185244-1/S0025-5718-1994-1185244-1.pdf]]
   */
  def fftms(x: BigInt, y: BigInt, N: BigInt) = {
  }
}

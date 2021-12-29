package Chainsaw

import Chainsaw.dspTest.TestMetric.TestMetric
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer

package object dspTest {

  object TestMetric extends Enumeration {
    type TestMetric = Value
    val SAME, PERMUTATION, APPROXIMATE = Value
  }

  def setMonitor[T <: BaseType](trigger: Bool, target: T, Container: ArrayBuffer[BigInt]) = fork {
    while (true) {
      if (trigger.toBoolean) Container += target.toBigInt
      sleep(2)
    }
  }

  def setMonitorOnVec[T <: BaseType](trigger: Bool, target: Vec[T], Container: ArrayBuffer[BigInt]) = fork {
    while (true) {
      if (trigger.toBoolean) Container ++= target.map(_.toBigInt)
      sleep(2)
    }
  }

  def pokeScalar(port: Data, data: Any) = {
    port match {
      case bool: Bool => bool #= data.asInstanceOf[Boolean]
      case bitVector: BitVector => bitVector #= data.asInstanceOf[BigInt]
      case sfix: SFix => sfix #= data.asInstanceOf[Double]
      case complexNumber: ComplexNumber => complexNumber #= data.asInstanceOf[BComplex]
    }
  }

  def pokeVec[D <: Data](vec: Vec[D], data: Any) = {
    vec.head match {
      case _: Bool => vec.asInstanceOf[Vec[Bool]].zip(data.asInstanceOf[Seq[Boolean]]).foreach { case (port, int) => port #= int }
      case _: BitVector => vec.asInstanceOf[Vec[BitVector]].zip(data.asInstanceOf[Seq[BigInt]]).foreach { case (port, int) => port #= int }
      case _: SFix => vec.asInstanceOf[Vec[SFix]].zip(data.asInstanceOf[Seq[Double]]).foreach { case (port, int) => port #= int }
      case _: ComplexNumber => vec.asInstanceOf[Vec[ComplexNumber]].zip(data.asInstanceOf[Seq[BComplex]]).foreach { case (port, int) => port #= int }
    }
  }

  /** poke data of whatever type(bool, int, double, complex), not type-safe(won't be found by linter)
   */
  def pokeWhatever(port: Data, data: Any): Unit = { // TODO: check whether it works for SInt
    port match {
      case fragment: Fragment[_] => fragment.fragment match {
        case vec: Vec[_] => pokeVec(vec, data)
        case scalar => pokeScalar(scalar, data)
      }
      case vec: Vec[_] => pokeVec(vec, data)
      case scalar => pokeScalar(scalar, data)
    }
  }

  def pokeZero[D](port: Data): Unit = {
    val complex0 = BComplex(0, 0)
    port match {
      //      case bool: Bool => bool #= false
      case bitVector: BitVector => bitVector #= 0
      case sfix: SFix => sfix #= 0
      case complexNumber: ComplexNumber => complexNumber #= complex0
      case vec: Vec[_] =>
        vec.head match {
          //          case bool: Bool => vec.asInstanceOf[Vec[Bool]].foreach(_ #= false)
          case bitVector: BitVector => vec.asInstanceOf[Vec[BitVector]].foreach(_ #= 0)
          case sfix: SFix => vec.asInstanceOf[Vec[SFix]].foreach(_ #= 0)
          case complexNumber: ComplexNumber => vec.asInstanceOf[Vec[ComplexNumber]].foreach(_ #= complex0)
        }
      case fragment: Fragment[_] =>
        fragment.fragment match {
          //          case bool: Bool => bool #= false
          case bitVector: BitVector => bitVector #= 0
          case sfix: SFix => sfix #= 0
          case complexNumber: ComplexNumber => complexNumber #= complex0
          case vec: Vec[_] =>
            vec.head match {
              //              case bool: Bool => vec.asInstanceOf[Vec[Bool]].foreach(_ #= false)
              case bitVector: BitVector => vec.asInstanceOf[Vec[BitVector]].foreach(_ #= 0)
              case sfix: SFix => vec.asInstanceOf[Vec[SFix]].foreach(_ #= 0)
              case complexNumber: ComplexNumber => vec.asInstanceOf[Vec[ComplexNumber]].foreach(_ #= complex0)
            }
        }
    }
  }

  /** peek data of whatever type(int, double, complex), not type-safe(won't by found by linter)
   */
  def peekWhatever[D](port: Data): D = {
    port match {
      case baseType: BaseType => baseType.toBigInt.asInstanceOf[D]
      case sfix: SFix => sfix.toDouble.asInstanceOf[D]
      case complexNumber: ComplexNumber => complexNumber.toComplex.asInstanceOf[D]
      case vec: Vec[_] =>
        vec.head match {
          case baseType: BaseType => vec.map(_.asInstanceOf[BaseType].toBigInt).asInstanceOf[D]
          case sfix: SFix => vec.map(_.asInstanceOf[SFix].toDouble).asInstanceOf[D]
          case complexNumber: ComplexNumber => vec.map(_.asInstanceOf[ComplexNumber].toComplex).asInstanceOf[D]
        }
      case fragment: Fragment[_] =>
        fragment.fragment match {
          case baseType: BaseType => baseType.toBigInt.asInstanceOf[D]
          case sfix: SFix => sfix.toDouble.asInstanceOf[D]
          case complexNumber: ComplexNumber => complexNumber.toComplex.asInstanceOf[D]
          case vec: Vec[_] =>
            vec.head match {
              case baseType: BaseType => vec.map(_.asInstanceOf[BaseType].toBigInt).asInstanceOf[D]
              case sfix: SFix => vec.map(_.asInstanceOf[SFix].toDouble).asInstanceOf[D]
              case complexNumber: ComplexNumber => vec.map(_.asInstanceOf[ComplexNumber].toComplex).asInstanceOf[D]
            }
        }
    }
  }


  /** A simple test procedure for Flow - poke stimulus to dataIn Flow and monitor dataOut Flow:
   *
   */
  def flowPeekPoke[Do, Di, Ti <: Data, To <: Data]
  (dut: Component, testCases: Seq[Di], dataIn: DataCarrier[Ti], dataOut: DataCarrier[To], latency: Int = 0): ArrayBuffer[Do] = {
    // init
    dataIn.clear()
    dataOut match {
      case stream: Stream[_] => stream.ready #= true
      case _ => // do nothing
    }
    dut.clockDomain.waitSampling()
    // set monitor
    val dutResult = ArrayBuffer[Do]()
    dataOut.setMonitor(dutResult)
    // poke stimulus
    var i = 0
    var cantPokePeriod = 0
    while (i < testCases.size) {
      val canPoke = dataIn match {
        case stream: Stream[_] => stream.ready.toBoolean
        case _ => true
      }
      if (cantPokePeriod > 1024) throw new IllegalStateException("simulation terminated as dut is not ready for over 1024 cycles")
      if (canPoke) {
        dataIn.poke(testCases(i), lastWhen = i == (testCases.length - 1))
        i += 1
        cantPokePeriod = 0
      } else {
        cantPokePeriod += 1
      }
      dut.clockDomain.waitSampling()
    }
    // wait for result
    dataIn.clear()
    dut.clockDomain.waitSampling(latency + 1)
    dutResult
  }

  def doFlowPeekPokeTest[Do, Di, Ti <: Data, To <: Data]
  (name: String, dut: => Component with DSPTestable[Ti, To],
   testCases: Seq[Di], golden: Seq[Do],
   initLength: Int = 0,
   testMetric: TestMetric = TestMetric.SAME, epsilon: Double = 1E-4): ArrayBuffer[Do] = {

    val logger: Logger = LoggerFactory.getLogger(s"dsptest-$name")

    val dutResult = ArrayBuffer[Do]()
    SimConfig.withWave
      .workspaceName(name)
      .compile(dut).doSim { dut =>

      val outputSize = dut.dataOut.payload match {
        case vec: Vec[_] => vec.size
        case _ => 1
      }
      val innerGolden: Seq[Do] = golden.drop(initLength * outputSize)

      import dut.{clockDomain, dataIn, dataOut, latency}
      dataIn.halt()
      dataOut.halt()
      clockDomain.forkStimulus(2)
      // TODO: for folded design of factor N, N - 1 idle cycles should be inserted to set the global counter to 0
      //      clockDomain.waitSampling(3)
      dutResult ++= flowPeekPoke(dut, testCases, dataIn, dataOut, latency).drop(initLength * outputSize)

      def scalar2string(scalar: Any) = scalar match {
        case bigInt: BigInt => bigInt.toString(16)
        case double: Double => double.toString()
        case complex: BComplex => complex.toString(6)
      }

      assert(dutResult.length == innerGolden.length, s"your result size = ${dutResult.length}, golden length = ${innerGolden.length}")

      if (innerGolden != null) {
        val printContent = dutResult.head match {
          case seq: Seq[_] => seq.head match {
            case _: BComplex => dutResult.indices.map(i => s"testing result $i:"
              + s"\nyours : ${dutResult(i).asInstanceOf[Seq[BComplex]].map(_.toString(6)).mkString(" ")}, sum = ${dutResult(i).asInstanceOf[Seq[BComplex]].map(_.real).sum * 2}"
              + s"\ngolden: ${innerGolden(i).asInstanceOf[Seq[BComplex]].map(_.toString(6)).mkString(" ")}, sum = ${innerGolden(i).asInstanceOf[Seq[BComplex]].map(_.real).sum * 2}"
              + s"\ndiff: ${dutResult(i).asInstanceOf[Seq[BComplex]].zip(innerGolden(i).asInstanceOf[Seq[BComplex]]).map { case (a, b) => (a.real - b.real).abs + (a.imag - b.imag).abs }.sum}").mkString("\n")
            case _: Double => dutResult.indices.map(i => s"testing result $i:"
              + s"\nyours : ${dutResult(i).asInstanceOf[Seq[Double]].map(_.toString).mkString(" ")}"
              + s"\ngolden: ${innerGolden(i).asInstanceOf[Seq[Double]].map(_.toString).mkString(" ")}"
              + s"\ndiff: ${dutResult(i).asInstanceOf[Seq[Double]].zip(innerGolden(i).asInstanceOf[Seq[Double]]).map { case (a, b) => (a - b).abs }.sum}").mkString("\n")
          }

          case _ =>
            val printSize = (dutResult ++ innerGolden).map(scalar2string).map(_.length).max
            val yourString = dutResult.map(scalar2string).map(_.padTo(printSize, ' ')).mkString(" ")
            val goldenString = golden.map(scalar2string).map(_.padTo(printSize, ' ')).mkString(" ")
            val diffString = yourString.zip(goldenString).map { case (a, b) => if (a == b) ' ' else 'x' }.mkString("")
            s"testing result:" +
              s"\nyours : $yourString" +
              s"\ngolden: $goldenString" +
              s"\ndiff  : $diffString"
        }

        logger.info(printContent)

        def shouldAll(metric: (Do, Do) => Boolean) = dutResult.zip(innerGolden).forall { case (a, b) => metric(a, b) }

        val condition: Boolean = testMetric match {
          case Chainsaw.dspTest.TestMetric.SAME => shouldAll(_ == _)
          // TODO: close, but not exactly the definition of permuataion
          case Chainsaw.dspTest.TestMetric.PERMUTATION => dutResult.diff(innerGolden).isEmpty && dutResult.size == innerGolden.size
          case Chainsaw.dspTest.TestMetric.APPROXIMATE => dutResult.head match {
            // FIXME: case 0 and 1 can't be viewed differently because of type erasure, it always fall on case 0
            // TODO: this frame work should always be 2-D
            case seq: Seq[_] => seq.head match {
              case _: Double => dutResult.asInstanceOf[ArrayBuffer[Seq[Double]]].flatten
                .zip(innerGolden.asInstanceOf[Seq[Seq[Double]]].flatten)
                .forall { case (a, b) => (a - b).abs < epsilon }
              case _: BComplex => dutResult.asInstanceOf[ArrayBuffer[Seq[BComplex]]].flatten
                .zip(innerGolden.asInstanceOf[Seq[Seq[BComplex]]].flatten)
                .forall { case (a, b) => (a.real - b.real).abs < epsilon && (a.imag - b.imag).abs < epsilon }
            }
            case _: BComplex => dutResult.asInstanceOf[ArrayBuffer[BComplex]]
              .zip(innerGolden.asInstanceOf[Seq[BComplex]])
              .forall { case (a, b) => (a.modulus - b.modulus).abs < epsilon }
            case _: Double => dutResult.asInstanceOf[ArrayBuffer[Double]]
              .zip(innerGolden.asInstanceOf[Seq[Double]])
              .forall { case (a, b) => (a - b).abs < epsilon }
            case _ => throw new IllegalArgumentException(s"'approximation' is not defined for ${dutResult.head.getClass}")
          }
        }
        assert(condition)
      }
    }
    dutResult
  }


  /** these methods help user's on testing Flow/Streams when the one-line test is not avilable
   *
   * @param dc Flow/Stream
   * @tparam T payload type
   */
  implicit class DataCarrierUtil[T <: Data](dc: DataCarrier[T]) {

    def halt(): Unit = {
      if (dc.valid.isInput) { // as slave
        dc.valid #= false

        dc.payload match {
          case fragment: Fragment[T] => fragment.last #= false
          // CAUTION: payload should remains the same
          //            fragment.fragment.randomize()
          //          case payload => payload.randomize()
          case _ => // do nothing
        }
      }
      else { // as master
        dc match {
          case stream: Stream[_] => stream.ready #= false
          case _ => // do nothing
        }
      }
    }

    def clear(): Unit = {
      if (dc.valid.isInput) { // as slave
        dc.valid #= false
        dc.payload match {
          case fragment: Fragment[T] => fragment.last #= false
            pokeZero(fragment.fragment)
          case payload => pokeZero(payload)
        }
      }
      else { // as master
        dc match {
          case stream: Stream[_] => stream.ready #= false
          case _ => // do nothing
        }
      }
    }

    // poke stimulus for all kinds of DataCarrier
    def poke[D](data: D, lastWhen: Boolean = false): Unit = {

      // deal with control signals
      dc.valid #= true
      dc.payload match {
        case fragment: Fragment[_] => fragment.last #= lastWhen
        case _ => // do nothing
      }

      // deal with payload
      pokeWhatever(dc.payload, data)
    }

    def pokeRandom(lastWhen: Boolean = false): Unit = {
      {

        // deal with control signals
        dc.valid #= true
        dc.payload match {
          case fragment: Fragment[_] => fragment.last #= lastWhen
          case _ => // do nothing
        }

        // deal with payload
        dc.payload.randomize()
      }
    }

    def forkWhenValid(body: => Unit): SimThread = fork {
      while (true) {
        if (dc.valid.toBoolean) {
          body
        }
        sleep(2)
      }
    }

    // set monitors for all kinds of DataCarrier
    def setMonitor[D](Container: ArrayBuffer[D], name: String = ""): SimThread = forkWhenValid {
      //      println(s"sampling $name at ${simTime()}")
      dc.payload match {
        case vec: Vec[_] => Container += peekWhatever(vec)
        case fragment: Fragment[_] =>
          fragment.fragment match {
            case vec: Vec[_] => Container += peekWhatever(vec)
            case _ => Container += peekWhatever(fragment.fragment)
          }
        case _ => Container += peekWhatever(dc.payload)
      }
    }
  }

  def buildCombBinary[THard <: Data](comb: (THard, THard) => THard, hardType: HardType[THard]) = {
    new Component {
      val x = in(hardType())
      val y = in(hardType())
      val ret = out(hardType())
      ret := comb(x, y)
    }
  }

  def evaluateCombBinary[THard <: Data, TSoft]
  (comb: (THard, THard) => THard, hardType: HardType[THard],
   Xs: Seq[TSoft], Ys: Seq[TSoft]): Seq[TSoft] = {
    val ret = ArrayBuffer[TSoft]()
    SimConfig.withWave.compile(buildCombBinary(comb, hardType)).doSim { dut =>
      Xs.indices.foreach { i =>
        pokeWhatever(dut.x, Xs(i))
        pokeWhatever(dut.y, Ys(i))
        sleep(1)
        ret += peekWhatever(dut.ret)
      }
    }
    ret
  }

}

package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._

import scala.collection.mutable.ArrayBuffer


package object dspTest {

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

  /** poke data of whatever type(int, double, complex), not type-safe(won't by found by linter)
   */
  def pokeWhatever[D](port: Data, data: D): Unit = {
    port match {
      case bitVector: BitVector => bitVector #= data.asInstanceOf[BigInt]
      case sfix: SFix => sfix #= data.asInstanceOf[Double]
      case complexNumber: ComplexNumber => complexNumber #= data.asInstanceOf[MComplex]
      case vec: Vec[_] =>
        vec.head match {
          case bitVector: BitVector => vec.asInstanceOf[Vec[BitVector]].zip(data.asInstanceOf[Seq[BigInt]]).foreach { case (port, int) => port #= int }
          case sfix: SFix => vec.asInstanceOf[Vec[SFix]].zip(data.asInstanceOf[Seq[Double]]).foreach { case (port, int) => port #= int }
          case complexNumber: ComplexNumber => vec.asInstanceOf[Vec[ComplexNumber]].zip(data.asInstanceOf[Seq[MComplex]]).foreach { case (port, int) => port #= int }
        }
      case fragment: Fragment[_] =>
        fragment.fragment match {
          case bitVector: BitVector => bitVector #= data.asInstanceOf[BigInt]
          case sfix: SFix => sfix #= data.asInstanceOf[Double]
          case complexNumber: ComplexNumber => complexNumber #= data.asInstanceOf[MComplex]
          case vec: Vec[_] =>
            vec.head match {
              case bitVector: BitVector => vec.asInstanceOf[Vec[BitVector]].zip(data.asInstanceOf[Seq[BigInt]]).foreach { case (port, int) => port #= int }
              case sfix: SFix => vec.asInstanceOf[Vec[SFix]].zip(data.asInstanceOf[Seq[Double]]).foreach { case (port, int) => port #= int }
              case complexNumber: ComplexNumber => vec.asInstanceOf[Vec[ComplexNumber]].zip(data.asInstanceOf[Seq[MComplex]]).foreach { case (port, int) => port #= int }
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

  /** A simple test procedure for Flow - poke stimulus to dataIn Flow and monitor dataOut Flow
   *
   */
  def flowPeekPokeRound[Do, Di, Ti <: Data, To <: Data](dut: Component, testCases: Seq[Di], dataIn: DataCarrier[Ti], dataOut: DataCarrier[To], latency: Int) = {
    // init
    dataIn.halt()
    dut.clockDomain.waitSampling()
    // set monitor
    val dutResult = ArrayBuffer[Do]()
    dataOut.setMonitor(dutResult)
    // poke stimulus
    testCases.indices.foreach { i =>
      dataIn.poke(testCases(i), lastWhen = i == (testCases.length - 1))
      dut.clockDomain.waitSampling()
    }
    // wait for result
    dataIn.halt()
    dut.clockDomain.waitSampling(latency + 1)
    dutResult
  }


  implicit class DataCarrierUtil[T <: Data](dc: DataCarrier[T]) {

    def halt() = {
      if (dc.valid.isInput) { // as slave
        dc.valid #= false
        dc.payload match {
          case fragment: Fragment[T] => fragment.last #= false
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

    // poke stimulus for all kinds of DataCarrier
    def poke[D](data: D, lastWhen: Boolean = false) = {

      // deal with control signals
      dc.valid #= true
      dc.payload match {
        case fragment: Fragment[_] => fragment.last #= lastWhen
        case _ => // do nothing
      }

      // deal with payload
      pokeWhatever(dc.payload, data)
    }

    def pokeRandom(lastWhen: Boolean = false) = {
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

    def forkWhenValid(body: => Unit) = fork {
      while (true) {
        if (dc.valid.toBoolean) {
          body
        }
        sleep(2)
      }
    }

    // set monitors for all kinds of DataCarrier
    def setMonitor[D](Container: ArrayBuffer[D], name:String = "") = forkWhenValid {
//      println(s"sampling $name at ${simTime()}")
      dc.payload match {
        case vec: Vec[_] => Container ++= peekWhatever(vec)
        case fragment: Fragment[_] =>
          fragment.fragment match {
            case vec: Vec[_] => Container ++= peekWhatever(vec)
            case _ => Container += peekWhatever(fragment.fragment)
          }
        case _ => Container += peekWhatever(dc.payload)
      }
    }
  }
}

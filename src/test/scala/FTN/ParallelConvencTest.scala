//package FTN
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.sim._
//import spinal.lib.fsm._
//
//import Chainsaw._
//import Chainsaw.Real
//
//import org.scalatest.funsuite.AnyFunSuite
//
//class ParallelConvencSim extends ParallelConvenc with DSPSimTiming[Bits, Bits, Array[Int], Array[Int]] {
//  override def poke(testCase: Array[Int], input: Bits): Unit = {
//    (0 until timing.inputInterval - 1).foreach { _ =>
//      input #= testCase
//      clockDomain.waitSampling()
//    }
//    input #= testCase
//  }
//  override def peek(output: Bits): Array[Int] = {
//    output.toBigInt.toString(2).reverse.padTo(896, '0').reverse.map(char => if(char == '1') 1 else 0).toArray
//  }
//  override def referenceModel(testCase: Array[Int]): Array[Int] = ???
//  override def isValid(refResult: Array[Int], dutResult: Array[Int]): Boolean = ???
//  override def messageWhenInvalid(testCase: Array[Int], refResult: Array[Int], dutResult: Array[Int]): String = ???
//  override def messageWhenValid(testCase: Array[Int], refResult: Array[Int], dutResult: Array[Int]): String = ???
//  override type RefOwnerType = this.type
//}
//
//class ParallelConvencTest extends AnyFunSuite {
//
//}

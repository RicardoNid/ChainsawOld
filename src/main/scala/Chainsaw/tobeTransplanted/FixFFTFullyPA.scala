//package Chainsaw.FloPoCo.BlackBoxed
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.sim._
//import spinal.lib.fsm._
//import Chainsaw._
//import Chainsaw.Real
//
//import scala.reflect.runtime.universe
//
//class FixFFTFullyPA(
//                     msbin: Int, lsbin: Int,
//                     msbout: Int, lsbout: Int,
//                     N: Int, radix: Int,
//                     signedIn: Boolean,
//                     decimation: Boolean,
//                     revbitorder: Boolean
//                   ) extends FloPoCoBlackBox[Vec[Real], Vec[Real]] {
//  override val input: Vec[Real] = in(Real())
//  override val output: Vec[Real] = out
//  override val operatorName: String = _
//
//  /** Just copy the line below, this is a must-be
//   *
//   * override def ruType = ru.typeOf[this.type]
//   */
//  override def ruType: universe.Type = ???
//
//  override type RefOwnerType = this.type
//}

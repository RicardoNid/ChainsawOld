package Chainsaw.crypto

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.symmetric._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

package object kyber {
  type int8   = Byte
  type int16  = Short
  type int32  = Int
  type int64  = Long
  type uint8  = Byte
  type uint16 = Short
  type uint32 = Int
  type uint64 = Long

  val sha256Rate   = 136
  val sha512Rate   = 72
  val shake256Rate = 136
  val shake128Rate = 168

  val q = 3329
  val n = 256
  val k = 2

  case class ploy() {
    var coef = new Array[Bits](n)
  }

  case class ployvec() {
    var vec = new Array[ploy](k)
  }

}

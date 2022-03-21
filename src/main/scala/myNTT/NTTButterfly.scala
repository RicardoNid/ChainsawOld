package myNTT

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable._
import scala.language.postfixOps
import scala.util.Random._

//Butterfly
//delay n+3

class NTTButterfly(N: BigInt /*被模常数*/ ) extends Component {

  val n = log2Up(N)

  val A = in UInt (n bits)
  val B = in UInt (n bits)
  val C = in UInt (n bits)
  val D = out UInt (n bits) //D=A+B
  val E = out UInt (n bits) //E=(A-B)*C*2^-n

  val ANext = RegNext(A)
  val BNext = RegNext(B)

  val addTemp = ANext +^ BNext
  val subTemp = ANext -^ BNext

  val input1 = addTemp.resize(n)
  val input3 = subTemp
  val input2 = addTemp - N
  val input4 = (subTemp + N).resize(n)

  val validAddInput = Mux(input2.msb, input1, input2.resized)
  val validSubInput = Mux(input3.msb, input4, input3.resized)

  val mulOutputArea = MontMul(RegNext(validSubInput), RegNext(RegNext(C)), N)

  val addBuffer = new buffer(UInt(n bits), n + 2) //delay

  addBuffer.dataIn := validAddInput

  D := addBuffer.dataOut
  E := mulOutputArea.output

  def getDelay: Int = n + 3

}

object NTTButterfly {
  def apply(N: BigInt, a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val butterfly = new NTTButterfly(N)
    butterfly.A := a
    butterfly.B := b
    butterfly.C := c
    (butterfly.D, butterfly.E)
  }
}

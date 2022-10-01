package Chainsaw.dsl

import spinal.core._
import spinal.lib._

case class RawImpl(impl: ((Vec[Bits], Bool)) => (Vec[Bits], Bool), latency: Int)

class ImplComponent(widthIn: Int, widthOut: Int,
                    sizeIn: Int, sizeOut: Int,
                    latency: Int, f: Vec[Bits] => Vec[Bits],
                    name: String) extends Component {
  val dataIn = in(Fragment(Vec(Bits(widthIn bits), sizeIn)))
  val dataOut = out(Fragment(Vec(Bits(widthOut bits), sizeOut)))
  dataIn.allowPruning()
  dataOut.allowPruning()
  dataOut.fragment := f(dataIn.fragment)
  dataOut.last := Delay(dataIn.last, latency, init = False)
  setDefinitionName(name)
}

object ImplComponent {
  def apply(widthIn: Int, widthOut: Int, sizeIn: Int, sizeOut: Int, latency: Int, f: Vec[Bits] => Vec[Bits], name: String): ImplComponent = new ImplComponent(widthIn, widthOut, sizeIn, sizeOut, latency, f, name)
}

abstract class Impl {

  val name: String
  val foldMax: Int = 1 // default value
  val width: (Int, Int)
  val size: (Int, Int) = (1, 1)

  def getFunction(fold: Int): Vec[Bits] => Vec[Bits]

  def getLatency(fold: Int): Int

  def getImpl(fold: Int) = ImplComponent(width._1, width._2, size._1, size._2, getLatency(fold), getFunction(fold), name)

}

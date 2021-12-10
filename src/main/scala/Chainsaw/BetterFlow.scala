package Chainsaw


import spinal.core._
import spinal.lib._

object BetterFlow {

  implicit class FlowUtil[T <: Data](flow: Flow[T]) {
    def arbitrationFrom[T2 <: Data](that: Flow[T2]) = flow.valid := that.valid
  }

  object FlowWidthAdapter {
    def apply[T <: Data, T2 <: Data](input: Flow[Fragment[T]], output: Flow[Fragment[T2]], endianness: Endianness = LITTLE, padding: Boolean = false): Unit = {
      val inputWidth = widthOf(input.fragment)
      val outputWidth = widthOf(output.fragment)

      if (inputWidth == outputWidth) { // connectFrom
        output.arbitrationFrom(input)
        output.payload.assignFromBits(input.payload.asBits)
      } else if (inputWidth > outputWidth) { // P2S, input should hold valid for N cycles
        require(inputWidth % outputWidth == 0 || padding)
        val factor = (inputWidth + outputWidth - 1) / outputWidth
        val paddedInputWidth = factor * outputWidth
        val counter = Counter(factor, inc = input.valid)
        output.valid := input.valid
        endianness match {
          case `LITTLE` => output.fragment.assignFromBits(input.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).read(counter))
          case `BIG` => output.fragment.assignFromBits(input.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
        }
        output.last := input.last && counter.willOverflowIfInc
      } else { // S2P
        require(outputWidth % inputWidth == 0 || padding)
        val factor = (outputWidth + inputWidth - 1) / inputWidth
        val paddedOutputWidth = factor * inputWidth
        val counter = Counter(factor, inc = input.valid)
        val buffer = Reg(Bits(paddedOutputWidth - inputWidth bits))
        when(input.valid) {
          buffer := input.fragment ## (buffer >> inputWidth)
        }
        output.valid := input.valid && counter.willOverflowIfInc
        endianness match {
          case `LITTLE` => output.fragment.assignFromBits((input.fragment ## buffer).resized)
          case `BIG` => output.fragment.assignFromBits((input.fragment ## buffer).subdivideIn(factor slices).reverse.asBits().resized)
        }
        output.last := input.last
      }
    }

    def make[T <: Data, T2 <: Data](input: Stream[Fragment[T]], outputPayloadType: HardType[T2], endianness: Endianness = LITTLE, padding: Boolean = false): Stream[Fragment[T2]] = {
      val ret = Stream(Fragment(outputPayloadType()))
      StreamFragmentWidthAdapter(input, ret, endianness, padding)
      ret
    }
  }

}

val logic = Seq(1.018, 1.169, 1.219, 1.225)
val net = Seq(1.706, 1.920, 2.232, 2.573)

val FPGA = logic.zip(net).map { case (d, d1) => d + d1 }
val ASIC = logic.zip(net).map { case (d, d1) => d + d1 / 2.0 }

println(FPGA.map(1 / _))
println(ASIC.map(1 / _))

import Chainsaw._

def value2C(number: BigInt, width:Int) = {
  val binary = number.toString(2).padToLeft(width, '0')
  -binary.head.asDigit * (BigInt(1) << (number.bitLength - 1)) + BigInt(binary.tail, 2)
}

value2C(BigInt("0011", 2), 4)

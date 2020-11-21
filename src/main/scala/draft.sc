import spinal.core._

val A = S("1011_0111")
//val B = A.roundToInf(6 bits, align = true) // sat 1 carry bit, got 10 bit
//val B = A.floor(6 bits) // return 10 bit
//val B = A.floorToZero(6 bits) // return 10 bit
//val B = A.ceil(6 bits, align = true) // ceil with carry then sat 1 bit return 10 bit
//
//val B0 = A.roundToInf(6 bits, align = true) //  ---+
////     |--> equal
//val B1 = A.roundToInf(6 bits, align = false).sat(1) //  ---+
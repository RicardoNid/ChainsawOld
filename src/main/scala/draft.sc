val temp = "0101"
temp.getBytes

val value = "0".getBytes
value(0).toBinaryString.reverse.padTo(8, '0').reverse

"0011000010101010"
  .grouped(8).toArray
  .map(_.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum.toByte)
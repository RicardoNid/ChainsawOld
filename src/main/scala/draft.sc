-15.toByte & 0xFF

137.toByte

new String("ab".getBytes)

"ab".getBytes
"AB".getBytes

BigInt("ab".getBytes) // 97 * 256 + 98
BigInt("Ab".getBytes) // 97 * 256 + 98

val temp = BigInt(Seq(-1, 3).map(_.toByte).toArray)
temp.toString(16)

BigInt("C4CBD2D9", 16)

BigInt("00", 16)

0xff ^ 0x10

 0xef ^ 0x98 ^ 0x10 ^ 0xf9
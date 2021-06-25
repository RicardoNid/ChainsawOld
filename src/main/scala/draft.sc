import scala.collection.mutable.ArrayBuffer

val e = BigInt(11)
val x = BigInt(2)

var p = BigInt(1)
var s = x

e.toString(2).reverse.foreach{ bit =>
  if(bit.asDigit == 1) p = p * s
  s = s * s
}



val a,b = ArrayBuffer[Int]()
a += 1
b
a == b
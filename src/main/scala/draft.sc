val e = BigInt(11)
val x = BigInt(2)

var p = BigInt(1)
var s = x

e.toString(2).reverse.foreach{ bit =>
  if(bit.asDigit == 1) p = p * s
  s = s * s
}

println(p)
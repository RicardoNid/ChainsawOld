val a = BigInt(13)
val n = a.bitLength
val aPrime = (a >> 1) + (1 << n - 1)
println(a.toString(2))
println(aPrime.toString(2))


val a = BigInt(1)
val b = BigInt(1)

def plus(value:BigInt) = BigInt((1 + value.toString(2)(0).asDigit).toBinaryString + value.toString(2).tail, 2)

plus(2)

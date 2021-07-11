def getHamming(a: Double, b: Double) = (BigInt(a.toInt) ^ BigInt(b.toInt)).toString(2).map(_.asDigit).sum

getHamming(1, 3)
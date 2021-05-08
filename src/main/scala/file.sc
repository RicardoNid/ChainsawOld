val a = 3.14159
BigDecimal(a)

scala.math.pow(2, -10)

def toUFixed(value: Double, maxExp: Int, minExp: Int) = {
  val shift = -minExp
  val ret = if (shift >= 0)
    (value * BigDecimal(BigInt(1) << shift)).toBigInt
  else
    (value / BigDecimal(BigInt(1) << -shift)).toBigInt

  val bs = ret.toString(2)
  "0" * (maxExp - minExp - bs.length) + bs
}

toUFixed(3.14, 2, -3)
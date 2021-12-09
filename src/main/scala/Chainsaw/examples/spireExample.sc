import spire.syntax.literals._

val x = b"100"
val y = h"100"
val mask = b"100"

val n1 = r"1/3"
val n2 = r"1599/115866"

import spire.syntax.literals.radix._

// representations of the number 23
val a = x2"10111" // binary
val b = x8"27" // octal
val c = x16"17" // hex

import spire.syntax.literals.si._ // .us and .eu also available

val w = i"1 944 234 123" // Int
val x = j"89 234 614 123 234 772" // Long
val y = big"123 234 435 456 567 678 234 123 112 234 345" // BigInt
val z = dec"1 234 456 789.123456789098765" // BigDecimal
import cc.redberry.rings.scaladsl._

val a = "561c342d 49a6daa3 704d4875 2e76c5a1 0fd64125 759d5488 362ee116 cbb54fd9 89c8d734 4fc3c31d 1e9eea08 8cae4574 8e74843c f5c7d905 998d2bcc 2b2e9e60".grouped(9).toSeq.reverse.map(_.replace(" ", "")).mkString("")
val aInt = BigInt(a, 16)

val b = "e6aba7f3 51d325ab 738147a9 d5acbad5 bba8f376 5f7d58d0 2c46a208 72adac38 d07d31b3 41048e22 7c0bd5d0 6e6b74f8 a358b34c 0b2d817a 8c1cb275 4d28c1f9 80000000".grouped(9).toSeq.reverse.map(_.replace(" ", "")).mkString("")
val bInt = BigInt(b, 16)

val M = "038d913f 4fdbd77a a06abed3 28f1b63c 73900ec2 560eaacf 40638c54 b8129662 e795fd17 95479808 c9b77441 23bb10d0 622ada77 cb2dd91c ffdb386d d4c986c3".grouped(9).toSeq.reverse.map(_.replace(" ", "")).mkString("")
val mInt = BigInt(M, 16)

Zp(mInt)(bInt) == Zp(mInt)(aInt)
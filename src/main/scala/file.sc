def bs2i(bs: String) = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum

bs2i("001")
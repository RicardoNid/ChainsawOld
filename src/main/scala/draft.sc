val temp = BigInt("10101010111100001", 2)

temp.toString(2).padTo(20, '0').grouped(4).toArray.map(wordString => BigInt(wordString.reverse, 2))
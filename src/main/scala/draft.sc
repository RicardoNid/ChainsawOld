def toWords(value: BigInt, w: Int, e: Int) =
  value.toString(2).reverse.padTo(w * e, '0').reverse.grouped(w).toSeq

toWords(39, 4, 3).mkString(" ")
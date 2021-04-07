import breeze.numerics.pow


def poly2trellis(structure: String) = {
  val octalDigits = structure.map(_.asDigit).reverse
  val value = (0 until octalDigits.length).map(i => pow(8, i) * octalDigits(i)).sum
  value.toBinaryString.reverse
}

poly2trellis("171")
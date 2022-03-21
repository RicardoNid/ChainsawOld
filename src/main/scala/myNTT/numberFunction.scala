package myNTT

object numberFunction {
  def reverse(input: Int, n: Int): Int = {
    var input_temp = input
    var output     = 0
    for (_ <- 0 until n) {
      output     = (output    << 1) | (1 & input_temp)
      input_temp = input_temp >> 1
    }
    output
  }
}

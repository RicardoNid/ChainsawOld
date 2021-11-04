package Chainsaw.comparith

class SequentialDivider {

}

object SequentialDivider {
  def software(z: BigInt, d: BigInt) = {
    val k = d.bitLength
    require(z < d * (BigInt(1) << k))
    var qString = ""
    def recursion(j: Int, partial: BigInt): BigInt = {
      if (j == k) partial >> k
      else {
        val temp = (partial << 1) - (d * (BigInt(1) << k))
        val newPartial = if (temp > 0) {
          qString += "1"
          temp
        } else {
          qString += "0"
          partial << 1
        }
        recursion(j + 1, newPartial)
      }
    }
    val s = recursion(0, z)
    (BigInt(qString, 2), s)
  }

  def softwareNonstoring(z: BigInt, d: BigInt) = {
    val k = d.bitLength
    require(z < d * (BigInt(1) << k))
    var qString = ""
    def recursion(j: Int, partial: BigInt): BigInt = {
      val newPartial = if (partial > 0) (partial << 1) - (d * (BigInt(1) << k))
      else (partial << 1) + (d * (BigInt(1) << k))
      if (j > 0) {
        if (partial > 0) qString += "1"
        else qString += "0"
      }
      if (j == k) if (partial < 0) (partial + d * (BigInt(1) << k)) >> k else partial >> k
      else recursion(j + 1, newPartial)
    }
    val s = recursion(0, z)
    (BigInt(qString, 2), s)
  }

//  def softwareNonstoringSigned(z: BigInt, d: BigInt) = {
//    val k = d.bitLength
//    require(z < d * (BigInt(1) << k))
//    var qString = ""
//    def recursion(j: Int, partial: BigInt): BigInt = {
//      val isSub = (partial > 0) ^ (d < 0)
//      val newPartial = if (isSub) (partial << 1) - (d * (BigInt(1) << k))
//      else (partial << 1) + (d * (BigInt(1) << k))
//      if (j > 0) {
//        if (isSub) qString += "1"
//        else qString += "0"
//      }
//      if (j == k) if (partial < 0) (partial + d * (BigInt(1) << k)) >> k else partial >> k
//      else recursion(j + 1, newPartial)
//    }
//    val s = recursion(0, z)
//    (BigInt(qString, 2), s)
//  }

  def main(args: Array[String]): Unit = {
    println(software(33, 7))
    println(softwareNonstoring(33, 7))
  }
}

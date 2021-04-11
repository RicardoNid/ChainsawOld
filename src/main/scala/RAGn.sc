import scala.collection.mutable

//  build the LUT
val coeff0 = Array(1, 2, 4, 8, 16, 32, 64, 128, 256)
val coeff1 = Array(3, 5, 6, 7, 9, 10, 12, 14, 15, 17, 18, 20, 24, 28, 30, 31, 33, 34, 36, 40, 48, 56, 60, 62, 63, 65, 66, 68, 72, 80, 96, 112, 120, 124, 126, 127, 129, 130, 132, 136, 144, 160, 192, 224, 240, 248, 252, 254, 255)
val coeff2 = Array(11, 13, 19, 21, 22, 23, 25, 26, 27, 29, 35, 37, 38, 39, 41, 42, 44, 46, 47, 49, 50, 52, 54, 55, 57, 58, 59, 61, 67, 69, 70, 71, 73, 74, 76, 78, 79, 81, 82, 84, 88, 92, 94, 95, 97, 98, 100, 104, 108, 110, 111, 113, 114, 116, 118, 119, 121, 122, 123, 125, 131, 133, 134, 135, 137, 138, 140, 142, 143, 145, 146, 148, 152, 156, 158, 159, 161, 162, 164, 168, 176, 184, 188, 190, 191, 193, 194, 196, 200, 208, 216, 220, 222, 223, 225, 226, 228, 232, 236, 238, 239, 241, 242, 244, 246, 247, 249, 250, 251, 253)
val coeff3 = Array(43, 45, 51, 53, 75, 77, 83, 85, 86, 87, 89, 90, 91, 93, 99, 101, 102, 103, 105, 106, 107, 109, 115, 117, 139, 141, 147, 149, 150, 151, 153, 154, 155, 157, 163, 165, 166, 167, 169, 170, 172, 174, 175, 177, 178, 180, 182, 183, 185, 186, 187, 189, 195, 197, 198, 199, 201, 202, 204, 206, 207, 209, 210, 212, 214, 215, 217, 218, 219, 221, 227, 229, 230, 231, 233, 234, 235, 237, 243, 245)
val coeff4 = Array(171, 173, 179, 181, 203, 205, 211, 213)
//  from http://kastner.ucsd.edu/fir-benchmarks/
val test0 = Array(3, 5, 17)
val test1 = Array(1, 7, 16, 21, 33)
val test2 = Array(6, 60, 343, 1374, 4124, 9624, 17873, 26810, 32768, 32768, 26810, 17873, 9624, 4124, 1374, 343, 60, 6, 0)
def getOddFundamental(n: Int) = {
  require(n > 0)
  var ret = n
  while (ret % 2 == 0) ret /= 2
  ret
}

def getOddFundamentals(numbers: Seq[Int]) = {
  numbers.filter(_ > 0).map(getOddFundamental).toSet.toSeq.sorted
}

val lut = mutable.Map[Int, Int]()
getOddFundamentals(coeff0).foreach(i => lut += (i -> 0))
getOddFundamentals(coeff1).foreach(i => lut += (i -> 1))
getOddFundamentals(coeff2).foreach(i => lut += (i -> 2))
getOddFundamentals(coeff3).foreach(i => lut += (i -> 3))

def lookup(n: Int) = lut.get(getOddFundamental(n)).getOrElse(-1)

def isAOperation(u: Int, v: Int, w: Int) = {

}

def AOperation(u: Int, v: Int, max: Int): mutable.Set[Int] = {
  require(u > 0 && v > 0)
  val ret = mutable.Set[Int]()
  var exp = 1
  //  TODO: cancel the while loop with a precomputer bound
  while ((2 << exp) * u + v <= max) {
    ret += (1 << exp) * u + v
    if ((2 << exp) * u - v > 0) ret += (2 << exp) * u - v
    exp += 1
  }
  exp = 1
  while ((1 << exp) * v + u <= max) {
    ret += (2 << exp) * v + u
    if ((2 << exp) * v - u > 0) ret += (2 << exp) * v - u
    exp += 1
  }
  var q = u + v
  while (q % 2 == 0) {
    ret += q / 2
    q /= 2
  }
  ret
}

//  step 1, 2, 3, 4
val incompleteSet = mutable.Map[Int, Int]()
val graphSet = mutable.Map[Int, Int]()
getOddFundamentals(test1).foreach { i =>
  val cost = lookup(i)
  cost match {
    case 0 => //  drop
    case 1 => graphSet += (i -> cost)
    case _ => incompleteSet += (i -> cost)
  }
}
incompleteSet
graphSet
val maxCoeff = test1.max
var added = true

//
while (incompleteSet.nonEmpty && added) {
  added = false
  //  TODO: optimize it as C_n^2 + n(upper triangular), not n^2
  val intersectionSet = mutable.Set[Int]()
  for (a <- graphSet; b <- graphSet) {
    if (a._1 >= b._1) {
      println(s"a: ${a._1}, b:${b._1}")
      val generatedSet = AOperation(a._1, b._1, maxCoeff)
      println(s"generated set: ${generatedSet.mkString(" ")}")
      val intersection = generatedSet.intersect(incompleteSet.keySet)
      println(s"intersection set: ${intersection.mkString(" ")}")
      intersectionSet ++= intersection
    }
  }
  if (intersectionSet.nonEmpty) added = true
  intersectionSet.foreach { coeff =>
    incompleteSet.remove(coeff)
    graphSet += (coeff -> 1)
  }
}
if (incompleteSet.isEmpty) println("optimal solution found")

incompleteSet
graphSet


//  step2

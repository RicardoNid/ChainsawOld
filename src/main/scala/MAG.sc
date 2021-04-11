import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

val filename = "/home/lsfans/LTRSpinal/src/main/scala/mag14.dat"
val goldenCostLUT = Source.fromFile(filename).getLines().mkString("")
  .zipWithIndex.map { case (c, i) => (i + 1) -> c.asDigit }.toMap

def compareWithGolden(yours: mutable.Map[Int, Int]) = {
  //  assert(yours.forall { case (coeff, cost) => goldenCostLUT.get(coeff).get == cost })
  yours.foreach { case (coeff, cost) =>
    val golden = goldenCostLUT.get(coeff).get
    if (golden != cost) println(s"yours: $coeff -> $cost, golden: $coeff -> $golden")
  }
  println(s"table built: ${yours.size} / 4096")
}

def getOddFundamental(n: Int) = {
  require(n > 0)
  var ret = n
  while (ret % 2 == 0) ret /= 2
  ret
}

def AOperation(u: Int, v: Int, max: Int): mutable.Set[Int] = {
  require(u > 0 && v > 0)
  val ret = mutable.Set[Int]()
  var exp = 1
  //  TODO: cancel the while loop with a precomputer bound
  while ((1 << exp) * u + v <= max) {
    ret += (2 << exp) * u + v
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
  ret += getOddFundamental(q)
  ret
}

val costLUT = mutable.Map[Int, Int]()
val fundamentalsLUT = mutable.Map[Int, ListBuffer[ListBuffer[Int]]]()

val range = 4096
//  step 2
var i = 1
while (i < range) {
  costLUT += i -> 0
  fundamentalsLUT += i -> ListBuffer(ListBuffer(1))
  i = i << 1
}
costLUT
//  step 3
AOperation(1, 1, range).foreach { coeff =>
  if (!costLUT.contains(coeff)) {
    costLUT += coeff -> 1
    fundamentalsLUT += coeff -> ListBuffer(ListBuffer(1, coeff))
  }
}
costLUT
//  step 4, building cost-2
costLUT.filter(_._2 == 1).foreach { case (coeff, cost) =>
  (AOperation(coeff, coeff, range) ++= AOperation(coeff, 1, range)).foreach { newCoeff =>
    if (!costLUT.contains(newCoeff)) {
      costLUT += newCoeff -> 2
      fundamentalsLUT += newCoeff -> ListBuffer(ListBuffer(1, coeff, newCoeff))
    }
    //  when a new path is found
    if (costLUT.contains(newCoeff) && costLUT.get(newCoeff).get == 2) {
      val fundamentals = fundamentalsLUT.get(newCoeff).get
      //  TODO: reconsider the risk of repeated path
      //  TODO: there surly exist repeted path
      //      if (fundamentals.forall(_ != ListBuffer(1, coeff, newCoeff)))
      fundamentals += ListBuffer(1, coeff, newCoeff)
    }
  }
}

fundamentalsLUT.foreach { case (i, buffer) =>
  val uniquePaths = buffer.distinct
  buffer.clear()
  buffer ++= uniquePaths
}

costLUT
compareWithGolden(costLUT)

println("multiple path example")
val example = fundamentalsLUT //  from the lookup table of fundamentals
  .filter(_._2.size > 1) //  get all the (coefficient,fundamental) who has more than one path
  .last._2 //  take the fundamentals of the last pair as an example
  .map(_.mkString("->")) //  for each path, make it a string, connecting by ->
  .mkString("\n") //  make all these strings one, seperated by \n


println(s"coefficient with multiple paths: ${fundamentalsLUT.filter(_._2.size > 1).size}")

//  step 5, building cost-3
//  searching method of patern 1 ~ 6
val tobeSearched = costLUT.count(_._2 == 2)
var searched = 0
costLUT.filter(_._2 == 2) //  for all the cost-2 coefficients
  .foreach { case (coeff, cost) =>
    val paths = fundamentalsLUT.get(coeff).get //  get all paths for a coefficient

    paths.foreach { path => //  for a specific path

      val newCoeffs = mutable.Set[Int]() //  prepare for new coefficients
      path.foreach { fundamental => //  for each fundamentals
        //        println(s"searching the pair $fundamental, $coeff")
        newCoeffs ++= AOperation(fundamental, coeff, range)
      } //  apply A-operation on each (fundamental, coefficient) pair to generate all candidate new coefficient

      newCoeffs.foreach { newCoeff => //  for each candidate
        val newPath = path ++ ListBuffer(newCoeff) //  if added, the path should be
        if (!costLUT.contains(newCoeff)) { //  not in cost LUT, just add it
          costLUT += newCoeff -> 3 //  ++ creates a new buffer, that's important
          fundamentalsLUT += newCoeff -> ListBuffer(newPath)
        }
        if (costLUT.contains(newCoeff) && costLUT.get(newCoeff).get == 3) { //  in cost LUT already, but the new paht is as short as the previous
          val paths = fundamentalsLUT.get(newCoeff).get //  get the previous paths
          //          if (paths.forall(_ != newPath))
          paths += newPath //  if it is truly new, add it
        }
      }
    }
    searched += 1
    //    println(s"$searched / $tobeSearched cost-2 design has been searched")
  }

fundamentalsLUT.foreach { case (i, buffer) =>
  val uniquePaths = buffer.distinct
  buffer.clear()
  buffer ++= uniquePaths
}
println(s"total paths number = ${fundamentalsLUT.map(_._2.size).sum}")

compareWithGolden(costLUT)

//  searching method of patern 7
val cost1coeffs = costLUT.filter(_._2 == 1).keys
for (coeff0 <- cost1coeffs; coeff1 <- cost1coeffs) { //  for all the cost-1 + cost-1
  if (coeff0 >= coeff1) { //  upper triangle, avoid same combination
    AOperation(coeff0, coeff1, range).foreach { newCoeff =>
      if (!costLUT.contains(newCoeff)) {
        println(newCoeff)
        costLUT += newCoeff -> 3
        fundamentalsLUT += newCoeff -> ListBuffer(ListBuffer(1, coeff0, coeff1, newCoeff))
      }
      //  when a new path is found
      if (costLUT.contains(newCoeff) && costLUT.get(newCoeff).get == 3) {
        val fundamentals = fundamentalsLUT.get(newCoeff).get
        fundamentals += ListBuffer(1, coeff0, coeff1, newCoeff)
      }
    }
  }
}

fundamentalsLUT.foreach { case (i, buffer) =>
  val uniquePaths = buffer.distinct
  buffer.clear()
  buffer ++= uniquePaths
}

compareWithGolden(costLUT)

println("multiple path example")
val example = fundamentalsLUT //  from the lookup table of fundamentals
  .filter(_._2.size > 1) //  get all the (coefficient,fundamental) who has more than one path
  .last._2 //  take the fundamentals of the last pair as an example
  .map(_.mkString("->")) //  for each path, make it a string, connecting by ->
  .mkString("\n") //  make all these strings one, seperated by \n

println(s"total paths number = ${fundamentalsLUT.map(_._2.size).sum}")

println(s"coefficient with multiple paths: ${fundamentalsLUT.filter(_._2.size > 1).size}")




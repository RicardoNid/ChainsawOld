package DSP

import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.math.min

object RAGn {

  val range = 16384
  val filename = "/home/lsfans/LTRSpinal/src/main/scala/mag14.dat" //  ground truth of cost LUT
  val goldenCostLUT = Source.fromFile(filename).getLines().mkString("")
    .zipWithIndex.map { case (c, i) => (i + 1) -> c.asDigit }.take(range).filter(pair => pair._1 % 2 != 0 || isPow2(pair._1))
    .toMap

  val costs = Array(0, 1, 2, 3, 4, 5).map(i => goldenCostLUT.filter(_._2 == i).size)

  val costLUT = mutable.Map[Int, Int]()
  val fundamentalsLUT = mutable.Map[Int, ListBuffer[ListBuffer[Int]]]()

  //  TODO: get LUT by reading files
  MAG() //  build the LUTs when algo object initialized

  def compareWithGolden(yours: mutable.Map[Int, Int]) = {
    //  assert(yours.forall { case (coeff, cost) => goldenCostLUT.get(coeff).get == cost })
    yours.foreach { case (coeff, cost) =>
      val golden = goldenCostLUT.get(coeff).get
      if (golden != cost) println(s"yours: $coeff -> $cost, golden: $coeff -> $golden")
    }
    println((0 until 5).map(i => s"cost-$i = ${yours.filter(_._2 == i).size} / ${costs(i)}").mkString("\n"))
    println(s"table built: ${yours.size} / ${goldenCostLUT.size}")
  }

  def showFundamentals(fundamentalsLUT: mutable.Map[Int, ListBuffer[ListBuffer[Int]]]) = {
    println(s"total paths number = ${fundamentalsLUT.map(_._2.size).sum}")
    println(s"coefficient with multiple paths: ${fundamentalsLUT.filter(_._2.size > 1).size}")
    println("multiple path example")
    val example = fundamentalsLUT //  from the lookup table of fundamentals
      .filter(_._2.size > 1) //  get all the (coefficient,fundamental) who has more than one path
      .last._2 //  take the fundamentals of the last pair as an example
      .map(_.mkString("->")) //  for each path, make it a string, connecting by ->
      .mkString("\n") //  make all these strings one, seperated by \n
    println(example)
  }

  def getOddFundamental(n: Int) = {
    require(n > 0)
    var ret = n
    while (ret % 2 == 0) ret /= 2
    ret
  }

  def getOddFundamentals(numbers: Seq[Int]) = {
    numbers.map(i => if (i < 0) -i else i).filter(_ != 0).map(getOddFundamental).toSet.toSeq.sorted
  }

  def lookup(n: Int) = costLUT.get(getOddFundamental(n)).getOrElse(-1)

  //  TODO: reconsider the searching range problem of A-operation
  def AOperation(u: Int, v: Int, max: Int): mutable.Set[Int] = {
    require(u > 0 && v > 0)
    val ret = mutable.Set[Int]()
    var exp = 1
    //  TODO: cancel the while loop with a precomputer bound
    while ((1 << exp) * u + v <= 2 * range || (1 << exp) * v + u <= 2 * range || (1 << exp) * u - v <= 2 * range || (1 << exp) * v - u <= 2 * range) {
      val cand0 = (1 << exp) * u + v
      val cand1 = (1 << exp) * u - v
      val cand2 = (1 << exp) * v + u
      val cand3 = (1 << exp) * v - u
      val validCand = Array(cand0, cand1, cand2, cand3).filter(cand => cand > 0 && cand <= max)
      validCand.foreach(ret += _)
      exp += 1
    }
    ret += getOddFundamental(u + v)
    ret
  }

  def MAG(): Unit = {
    //  step 2
    var i = 1
    while (i <= range) {
      costLUT += i -> 0
      fundamentalsLUT += i -> ListBuffer(ListBuffer(1))
      i = i << 1
    }
    compareWithGolden(costLUT)
    //  step 3, building cost-1
    AOperation(1, 1, range).foreach { coeff =>
      if (!costLUT.contains(coeff)) {
        costLUT += coeff -> 1
        fundamentalsLUT += coeff -> ListBuffer(ListBuffer(1, coeff))
      }
    }
    compareWithGolden(costLUT)
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
          fundamentals += ListBuffer(1, coeff, newCoeff)
        }
      }
    }

    fundamentalsLUT.foreach { case (i, buffer) => //  eliminate repeted paths
      val uniquePaths = buffer.distinct
      buffer.clear()
      buffer ++= uniquePaths
    }
    showFundamentals(fundamentalsLUT)
    compareWithGolden(costLUT)

    //  step 5, building cost-3

    //  searching method of patern 1 ~ 6
    costLUT.filter(_._2 == 2) //  for all the cost-2 coefficients
      .foreach { case (coeff, cost) =>
        val paths = fundamentalsLUT.get(coeff).get //  get all paths for a coefficient

        paths.foreach { path => //  for a specific path
          val newCoeffs = mutable.Set[Int]() //  prepare for new coefficients
          path.foreach { fundamental => //  for each fundamentals
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
              paths += newPath //  if it is truly new, add it
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
    showFundamentals(fundamentalsLUT)

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
    showFundamentals(fundamentalsLUT)

    //  step 6 building cost-4

    //  searching method of patern 1 ~ 27
    costLUT.filter(_._2 == 3) //  for all the cost-3 coefficients
      .foreach { case (coeff, cost) =>
        val paths = fundamentalsLUT.get(coeff).get //  get all paths for a coefficient

        paths.foreach { path => //  for a specific path
          val newCoeffs = mutable.Set[Int]() //  prepare for new coefficients
          path.foreach { fundamental => //  for each fundamentals
            newCoeffs ++= AOperation(fundamental, coeff, range)
          } //  apply A-operation on each (fundamental, coefficient) pair to generate all candidate new coefficient

          newCoeffs.foreach { newCoeff => //  for each candidate
            val newPath = path ++ ListBuffer(newCoeff) //  if added, the path should be
            if (!costLUT.contains(newCoeff)) { //  not in cost LUT, just add it
              costLUT += newCoeff -> 4 //  ++ creates a new buffer, that's important
              fundamentalsLUT += newCoeff -> ListBuffer(newPath)
            }
            if (costLUT.contains(newCoeff) && costLUT.get(newCoeff).get == 4) { //  in cost LUT already, but the new paht is as short as the previous
              val paths = fundamentalsLUT.get(newCoeff).get //  get the previous paths
              paths += newPath //  if it is truly new, add it
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
    showFundamentals(fundamentalsLUT)

    //  searching method of patern 28,29,32
    val cost2coeffs = costLUT.filter(_._2 == 2).keys
    for (coeff0 <- cost1coeffs; coeff1 <- cost2coeffs) { //  for all the cost-1 + cost-1
      //        println(s"pair $coeff0, $coeff1")
      AOperation(coeff0, coeff1, range).foreach { newCoeff =>
        if (!costLUT.contains(newCoeff)) {
          costLUT += newCoeff -> 4
          fundamentalsLUT += newCoeff -> ListBuffer(ListBuffer(1, coeff0, coeff1, newCoeff))
        }
        //  when a new path is found
        if (costLUT.contains(newCoeff) && costLUT.get(newCoeff).get == 4) {
          val fundamentals = fundamentalsLUT.get(newCoeff).get
          fundamentals += ListBuffer(1, coeff0, coeff1, newCoeff)
        }
      }
    }

    fundamentalsLUT.foreach { case (i, buffer) =>
      val uniquePaths = buffer.distinct
      buffer.clear()
      buffer ++= uniquePaths
    }
    compareWithGolden(costLUT)
    showFundamentals(fundamentalsLUT)

    // TODO: implement pattern 30, 31
  }

  def RAGn(coefficients: Seq[Int]): (mutable.ListBuffer[Int], Boolean) = {
    val incompleteSet = mutable.Set[Int]()
    val graphSet = mutable.ListBuffer[Int](1)

    val cost1 = costLUT.filter(_._2 == 1).keySet.toSeq.sorted //  sort it as smaller fundamentals have higher priority to be a building block

    def showStatus = {
      println(s"current incomplete set: ${incompleteSet.mkString(" ")}")
      println(s"current graph set: ${graphSet.mkString(" ")}")
    }

    //  step 1-4, initialization
    getOddFundamentals(coefficients).foreach { coeff =>
      val cost = lookup(coeff)
      cost match {
        case 0 => //  drop
        case 1 => graphSet += coeff
        case _ => incompleteSet += coeff
      }
    }
    showStatus

    val maxCoeff = coefficients.max
    var addDistance1 = true
    var addDistance2 = true
    var optimal = true

    while (incompleteSet.nonEmpty) {
      while (incompleteSet.nonEmpty && addDistance2) {
        //  step 5-6, repeatedly add distance-1 coefficients
        while (incompleteSet.nonEmpty && addDistance1) {
          addDistance1 = false
          val intersectionSet = mutable.Set[Int]()
          for (a <- graphSet; b <- graphSet) {
            if (a >= b) { //  upper triangle
              val generatedSet = AOperation(a, b, maxCoeff)
              printlnWhenDebug(s"generated set: ${generatedSet.mkString(" ")}")
              val intersection = generatedSet.intersect(incompleteSet)
              printlnWhenDebug(s"intersection set: ${intersection.mkString(" ")}")
              intersectionSet ++= intersection
            }
          }
          if (intersectionSet.nonEmpty) addDistance1 = true
          intersectionSet.foreach { coeff => //  move intersections from imcomplete set to graph set
            incompleteSet.remove(coeff)
            if (graphSet.contains(coeff)) graphSet += coeff
          }
        }

        if (incompleteSet.isEmpty) return (graphSet, optimal)

        //  heuristic part

        //  step 7, add distance-2 coefficients
        //  coefficient of interest and the auxiliary coefficient, both of them should be added
        optimal = false //  once enter heuristic part, the solution is not asserted to be optimal
        addDistance2 = false
        var newDistance2 = Int.MaxValue
        var newAuxiliary = 1
        for (cost1coeff <- cost1; implemented <- graphSet) { //  pattern 1: cost-1 + implemented coefficient
          println(s"current cost-1 auxiliary: $cost1coeff, implemented: $implemented")

          println(s"candidates: ${AOperation(cost1coeff, implemented, maxCoeff).mkString(" ")}")
          val candidates = AOperation(cost1coeff, implemented, maxCoeff) //  all candidates generated through an A-operation
            .intersect(incompleteSet) //  find coefficients of interest
          if (candidates.nonEmpty) {
            val candidate = candidates.min
            if (candidate < newDistance2) { //  update the minimum and its auxiliary
              newDistance2 = candidate
              newAuxiliary = cost1coeff
            }
            if (candidate == newDistance2) newAuxiliary = min(newAuxiliary, cost1coeff)
          }
        }
        for (implemented0 <- graphSet; implemented1 <- graphSet) { //  pattern 2: cost-0 + sum of two implemented coefficient
          if (implemented0 >= implemented1) { //  upper triangle
            val auxiliary = implemented0 + implemented1
            val candidates = AOperation(1, implemented0 + implemented1, maxCoeff).intersect(incompleteSet)
            if (candidates.nonEmpty) {
              val candidate = candidates.min
              if (candidate < newDistance2) {
                newDistance2 = candidate
                newAuxiliary = auxiliary
              }
              if (candidate == newDistance2) newAuxiliary = min(newAuxiliary, auxiliary)
            }
          }
        }
        incompleteSet.remove(newDistance2)
        if (newDistance2 != Int.MaxValue) {
          if (!graphSet.contains(newAuxiliary)) graphSet += newAuxiliary
          if (!graphSet.contains(newDistance2)) graphSet += newDistance2
          addDistance2 = true
        }

        if (incompleteSet.isEmpty) return (graphSet, optimal)
      }

      //  step 9 add coefficient of distance-3 and more
      val minCost = incompleteSet.map(lookup).min
      val newCoeff = incompleteSet.filter(lookup(_) == minCost).min
      val newPath = fundamentalsLUT.get(newCoeff).get.sortBy(_.intersect(graphSet).size).last //  path with most fundamentals implemented
      //      val newPath = fundamentalsLUT.get(newCoeff).get.sortBy(_.sum).head //  path with smallest sum of fundamentals
      incompleteSet.remove(newCoeff)
      newPath.foreach(fundamental => if (!graphSet.contains(fundamental)) graphSet += fundamental)

      if (incompleteSet.isEmpty) return (graphSet, optimal)
    }
    return (graphSet, optimal)
  }

  def main(args: Array[String]): Unit = {

    //    val test1 = Array(1, 7, 16, 21, 33)
    //    println(RAGn(test1).mkString(" "))

    //    val test2 = Array(346, 208, -44, 9)
    //    val result = RAGn(test2)
    //    println(s"${result._1.mkString(" ")} \n optimal solution: ${result._2}")

    println(fundamentalsLUT.get(1245).get.map(_.mkString(" ")).mkString("\n"))

    //    val test3 = Array(16384, 5769, 1245, 7242, 13, 1548, 798)
    //    val result = RAGn(test3)
    //    println(s"${result._1.mkString(" ")} \n optimal solution: ${result._2}")

    //    println(AOperation(3, 11, 173))
  }
}

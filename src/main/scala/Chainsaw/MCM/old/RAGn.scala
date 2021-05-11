package Chainsaw.MCM.old

import Chainsaw.MCM.ASSSign.{ADD, SUBNEXT, SUBPREV}
import Chainsaw.MCM.{AConfigVector, AOperation}
import Chainsaw.{MCM, debug, printlnWhenDebug}
import breeze.numerics.abs
import org.jgrapht.traverse.DepthFirstIterator
import spinal.core.{isPow2, log2Up}

import java.io._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object RAGn {

  val range = 16384
  val filename = "src/main/resources/mag14.dat" //  ground truth of MAG cost
  val goldenCostLUT = Source.fromFile(filename).getLines().mkString("")
    .zipWithIndex.map { case (c, i) => (i + 1) -> c.asDigit }.take(range).filter(pair => pair._1 % 2 != 0)
    .toMap

  val costs = Array(0, 1, 2, 3, 4, 5).map(i => goldenCostLUT.filter(_._2 == i).size)

  //  TODO: get LUT by reading files
  val costLUTFileName = "src/main/resources/14bitCostLUT.dat"
  val fundamentalsLUTFileName = "src/main/resources/14bitFundamentalLUT.dat"
  val costLUTFile = new File(costLUTFileName)
  val fundamentalsLUTFile = new File(fundamentalsLUTFileName)
  var costLUT = mutable.Map[Int, Int]() //  coeffcient -> optimal cost pair
  var fundamentalsLUT = mutable.Map[Int, ListBuffer[ListBuffer[Int]]]()

  val runMAG = debug || !costLUTFile.exists() || !costLUTFile.exists()
  if (!runMAG) {
    val ois0 = new ObjectInputStream(new FileInputStream(costLUTFileName))
    val ois1 = new ObjectInputStream(new FileInputStream(fundamentalsLUTFileName))
    costLUT = ois0.readObject.asInstanceOf[mutable.Map[Int, Int]]
    fundamentalsLUT = ois1.readObject.asInstanceOf[mutable.Map[Int, ListBuffer[ListBuffer[Int]]]]
    ois0.close()
    ois1.close()
  }
  else { // MAG result serialization
    val oos0 = new ObjectOutputStream(new FileOutputStream(costLUTFileName))
    val oos1 = new ObjectOutputStream(new FileOutputStream(fundamentalsLUTFileName))
    MAG()
    oos0.writeObject(costLUT)
    oos1.writeObject(fundamentalsLUT)
    oos0.close()
    oos1.close()
  }

  def addToLUT(coeff: Int, cost: Int, path: ListBuffer[Int]) = {
    if (!costLUT.contains(coeff)) { //  if it's not implemented yet, add it
      costLUT += coeff -> cost
      fundamentalsLUT += coeff -> ListBuffer(path)
    }
    else if (lookupCost(coeff) == cost) lookupPaths(coeff) += path //  //  already implemented with same cost, add a new path
    //  already implemented at lower cost, drop it
  }

  def compareWithGolden = {
    //  assert(yours.forall { case (coeff, cost) => goldenCostLUT.get(coeff).get == cost })
    costLUT.foreach { case (coeff, cost) =>
      val golden = goldenCostLUT.get(coeff).get
      if (golden != cost) println(s"yours: $coeff -> $cost, golden: $coeff -> $golden")
    }
    println((0 until 5).map(i => s"cost-$i = ${costLUT.filter(_._2 == i).size} / ${costs(i)}").mkString("\n"))
    println(s"table built: ${costLUT.size} / ${goldenCostLUT.size}")
  }

  def showFundamentals = {
    println(s"total paths number = ${fundamentalsLUT.map(_._2.size).sum}")
    println(s"coefficient with multiple paths: ${fundamentalsLUT.filter(_._2.size > 1).size}")
    println("--------------------------------")
  }

  def removeRepeatedFundamentals = {
    fundamentalsLUT.map { case (i, buffer) => //  eliminate repeted paths
      val result = ListBuffer[ListBuffer[Int]]()
      val seen = mutable.HashSet[ListBuffer[Int]]()
      buffer.foreach { path =>
        if (!seen(path.sorted)) {
          result += path
          seen += path.sorted
        }
      }
      result
    }
  }

  def postProcess = {
    removeRepeatedFundamentals
    compareWithGolden
    showFundamentals
  }

  def getPositiveOddFundamental(n: Int) = {
    require(n != 0)
    var ret = if (n < 0) -n else n //  positive
    while (ret % 2 == 0) ret /= 2 // odd
    ret
  }

  def getPositiveOddFundamentals(numbers: Seq[Int]): Seq[Int] = numbers.filter(_ != 0).map(getPositiveOddFundamental).toSet.toSeq.sorted

  def showpath(coeff: Int) = println(lookupPaths(coeff).map(_.mkString("->")).mkString("\n"))

  def lookupCost(n: Int) = costLUT.get(getPositiveOddFundamental(n)).getOrElse(-1)

  def lookupPaths(n: Int) = fundamentalsLUT.get(getPositiveOddFundamental(n)).get

  def costNcoeffs(n: Int) = costLUT.filter(_._2 == n).keys

  def ASet(u: Int, v: Int, max: Int): mutable.Set[Int] = {
    require(u > 0 && v > 0 && u % 2 != 0 && v % 2 != 0) //  positive odd fundamentals
    val ret = mutable.Set[Int]() //  reachable coefficients
    var exp = 1
    var continue = true
    while (continue) { //  situation 1 & 2, j = 0, k > 0 or j >0, k = 0
      val cands = Array( //  1 << exp stands for 2^i
        (1 << exp) * u + v, (1 << exp) * u - v, v - (1 << exp) * u,
        (1 << exp) * v + u, (1 << exp) * v - u, u - (1 << exp) * v)
      val validCand = cands.filter(cand => cand > 0 && cand <= max)
      validCand.foreach(ret += _)
      continue = validCand.map(_ * 2).exists(_ < max)
      exp += 1
    }
    ret += getPositiveOddFundamental(u + v) //  situation 3, j = k < 0
    if (u != v) ret += getPositiveOddFundamental(if (u > v) u - v else v - u)
    ret
  }

  def AVectors(u: Int, v: Int, max: Int) = {
    require(u > 0 && v > 0 && u % 2 != 0 && v % 2 != 0) //  positive odd fundamentals
    val ret = mutable.Map[Int, AConfigVector]() //  reachable coefficients
    var exp = 1
    var continue = true
    while (continue) { //  situation 1 & 2, j = 0, k > 0 or j >0, k = 0
      val cands = Array( //  1 << exp stands for 2^i
        (1 << exp) * u + v -> AConfigVector(exp, 0, 0, ADD),
        (1 << exp) * u - v -> AConfigVector(exp, 0, 0, SUBNEXT),
        -((1 << exp) * u - v) -> AConfigVector(exp, 0, 0, SUBPREV),
        (1 << exp) * v + u -> AConfigVector(0, exp, 0, ADD),
        (1 << exp) * v - u -> AConfigVector(0, exp, 0, SUBPREV),
        -((1 << exp) * v - u) -> AConfigVector(0, exp, 0, SUBNEXT))
      val validCand = cands.filter(cand => cand._1 > 0 && cand._1 <= max)
      validCand.foreach(ret += _)
      continue = validCand.map(_._1 * 2).exists(_ < max)
      exp += 1
    }
    ret += getPositiveOddFundamental(u + v) -> AConfigVector(0, 0, log2Up((u + v) / getPositiveOddFundamental(u + v)), ADD) //  situation 3, j = k < 0
    val diff = if (u > v) u - v else v - u
    val sign = if (u > v) SUBNEXT else SUBPREV
    if (u != v) ret += getPositiveOddFundamental(diff) -> AConfigVector(0, 0, log2Up(diff / getPositiveOddFundamental(diff)), sign)
    ret
  }

  def AReverse(w: Int, u: Int, v: Int) = {
    require(w > 0 && u > 0 && v > 0 && w % 2 == 0 && u % 2 != 0 && v % 2 != 0, "fundamentals should be preprocessed into positive odd")
    if (w == getPositiveOddFundamental(u + v)) Some((0, 0, log2Up((u + v) / w)))
    else {
      var found = false
      while (!found) {

      }
    }
  }

  def mergePaths(path0: ListBuffer[Int], path1: ListBuffer[Int], newCoeff: Int) = {
    require(path0.head == 1 && path1.head == 1)
    if (path0.drop(1).intersect(path1.drop(1)).nonEmpty) None
    else Some(ListBuffer(1) ++= path0.drop(1) ++= path1.drop(1) ++= ListBuffer(newCoeff))
  }

  def mergePathLists(paths0: ListBuffer[ListBuffer[Int]], paths1: ListBuffer[ListBuffer[Int]], newCoeff: Int) = {
    val ret = ListBuffer[ListBuffer[Int]]()
    for (path0 <- paths0; path1 <- paths1) {
      mergePaths(path0, path1, newCoeff) match {
        case Some(path) => ret += path
      }
    }
    ret
  }

  def ASetOnSets[T <: Iterable[Int]](U: T, V: T, max: Int, asymmetric: Boolean = true) = {
    val ret = mutable.Set[Int]() //  generated coefficietn -> set of (u,v) s
    for (u <- U; v <- V) if (asymmetric || u >= v) ret ++= ASet(u, v, max)
    ret
  }

  def apply(coefficients: Seq[Int]): (mutable.LinkedHashSet[Int], Boolean, MCM.AdderGraph) = {
    val incompleteSet = mutable.Set[Int]()
    var graphSet = mutable.LinkedHashSet[Int](1)
    val resultAG = new MCM.AdderGraph()

    def showStatus = {
      println(s"  current incomplete set: ${incompleteSet.mkString(" ")}")
      println(s"  current graph set: ${graphSet.mkString(" ")}")
    }

    //  step 1-4, initialization
    getPositiveOddFundamentals(coefficients).foreach { coeff =>
      lookupCost(coeff) match {
        case 0 => //  drop
        case 1 => {
          graphSet += coeff
          val exp = if (isPow2(coeff + 1)) log2Up(coeff + 1) else log2Up(coeff - 1) // since cost-1 value = 2^i \pm 1
          val sign = if (isPow2(coeff + 1)) SUBNEXT else ADD
          if (resultAG.addFundamental(1, 1, AOperation(exp, 0, 0, sign))) printlnWhenDebug(s"$coeff successfully added")
        }
        case _ => incompleteSet += coeff
      }
    }
    println("initialization done")
    showStatus

    val maxCoeff = coefficients.max
    var distance1Added = true
    var distance2Added = true
    var optimal = true

    def addDistance1 = {
      var found = false
      for (impl0 <- graphSet; impl1 <- graphSet) {
        if (impl0 >= impl1) { //  upper tri
          //  TODO: consider the situation of multiple distance-1 implementations on the same coefficient
          AVectors(impl0, impl1, maxCoeff)
            .filter { case (coeff, vector) => incompleteSet.contains(coeff) } //intersections with incompleteSet
            .foreach { case (coeff, vector) =>
              found = true
              incompleteSet -= coeff
              graphSet += coeff
              println(impl0, impl1, vector)
              if (!resultAG.containsFundamental(coeff) && resultAG.addFundamental(impl0, impl1, AOperation(vector))) printlnWhenDebug(s"$coeff successfully added ")
            }
        }
      }
      if (found) {
        println("add distance-1 coeffs")
        showStatus
      }
      found
    }

    def addDistance2 = {
      val candidatePairs = mutable.Set[Tuple3[Int, Int, Int]]() //  candidate (newly implemented coeff, auxiliary, implemented coeff) pairs
      for (cost1 <- costNcoeffs(1); implemented <- graphSet) { //  pattern 1: cost-1 + implemented coefficient
        val candidates = ASet(cost1, implemented, maxCoeff) //  all candidates generated through an A-operation
          .intersect(incompleteSet) //  find coefficients of interest
        candidates.foreach(coeff => candidatePairs += Tuple3(coeff, cost1, implemented)) //  add new pair to the candidate set
      }
      for (implemented0 <- graphSet; implemented1 <- graphSet) { //  pattern 2: cost-0 + sum of two implemented coefficient
        if (implemented0 >= implemented1) { //  upper triangle
          val auxiliary = getPositiveOddFundamental(implemented0 + implemented1)
          val candidates = ASet(1, auxiliary, maxCoeff).intersect(incompleteSet)
          candidates.foreach(coeff => candidatePairs += Tuple3(coeff, auxiliary, implemented0))
        }
      }
      val found = candidatePairs.nonEmpty
      if (found) {
        val minCoeff = candidatePairs.map(_._1).min
        val minAuxiliary = candidatePairs.filter(_._1 == minCoeff).map(_._2).min
        val implemented = candidatePairs.filter(_._1 == minCoeff).filter(_._2 == minAuxiliary).head._3
        incompleteSet.remove(minCoeff)
        graphSet += minAuxiliary //  coefficient of interest and the auxiliary coefficient, both of them should be added
        graphSet += minCoeff
        //        if (lookupCost(minAuxiliary) == 1) { // pattern 1
        //          resultAG.addFundamental(1, 1, minAuxiliary) // add the cost-1 aux
        //          resultAG.addFundamental(minAuxiliary, implemented, minCoeff) // add the new coefficient
        //        }
        //        else { // pattern 2
        //          val anotherImplementd = // find another implemented
        //        }
        println("add a distance-2 coeff")
        showStatus
      }
      found
    }

    def addDistance3 = {
      val minCost = incompleteSet.map(lookupCost).min
      val newCoeff = incompleteSet.filter(lookupCost(_) == minCost).min
      val maxImplemented = lookupPaths(newCoeff).map(_.toSet.intersect(graphSet).size).max
      val shortest = lookupPaths(newCoeff).filter(_.toSet.intersect(graphSet).size == maxImplemented) //  paths with most fundamentals implemented, thus shortest
      val newPath = shortest.sortBy(_.toSet.diff(graphSet).sum).head //  path with smallest sum of unimplemented fundamentals among shortest ones
      incompleteSet.remove(newCoeff)
      graphSet ++= newPath
      //      graphSet = graphSet.distinct
      println(s"add a distance-$minCost coeff")
      showStatus
    }

    def createOutput = {
      coefficients.foreach { coeff =>
        val OF = getPositiveOddFundamental(coeff)
        val exp = log2Up(abs(coeff / OF))
        resultAG.addOutput(OF, exp, coeff < 0)
      }
    }

    while (incompleteSet.nonEmpty) {
      distance2Added = true
      while (incompleteSet.nonEmpty && distance2Added) {
        //  optimal part
        distance1Added = true
        while (incompleteSet.nonEmpty && distance1Added) distance1Added = addDistance1 //  step 5-6, repeatedly add distance-1 until no one can be found
        if (incompleteSet.isEmpty) {
          createOutput
          return (graphSet, optimal, resultAG)
        }
        //  heuristic part
        optimal = false //  once enter heuristic part, the solution is not asserted to be optimal
        distance2Added = addDistance2 //  step 7, repeatedly add distance-2 coefficients
        if (incompleteSet.isEmpty) {
          createOutput
          return (graphSet, optimal, resultAG)
        }
      }
      //  step 9 add coefficient of distance-3 and more
      addDistance3
      if (incompleteSet.isEmpty) {
        createOutput
        return (graphSet, optimal, resultAG)
      }
    }
    createOutput
    return (graphSet, optimal, resultAG)
  }

  def main(args: Array[String]): Unit = {
    val test = Array(16384, 5769, 1245, 7242, 13, 1548, 798)
    //    val test = Array(346, 208, -44, 9)
    //    val test1 = Array()
    //    val test = Array(3, 13, 39, 59, 173)
    //    val test = Array(3, 13, 39, 59, 173)
    //    println(AVectors(3, 3, 128).mkString("\n"))
    val coe = Source.fromFile("ex2PM16_119.coe").getLines().drop(1) map (_.filter(_.isDigit).toInt)
    val result = apply(coe.toSeq)
    //    val result = RAGn(test)
    println(s"--------------------------------\n${result._1.mkString(" ")}\noptimal solution: ${result._2}\n--------------------------------")
    val dfsIter = new DepthFirstIterator(result._3.graph)
    println(s"DFS: ${dfsIter.toSeq.map(_.value).mkString("->")}")
    //    println(AVectors(3, 7, 128).mkString("\n"))
  }
}

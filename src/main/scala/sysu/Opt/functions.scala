package sysu.Opt

import sysu.util.printArray2D

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.mutable.{SortedSet => Set}

object functions {

  def seqRelation(seq0: Array[Int], seq1: Array[Int]) = {
    val offset = seq0(0) - seq1(0)
    if (seq0.zip(seq1.map(_ + offset)).forall { case (ele0, ele1) => ele0 == ele1 }) ("offset", offset)
    else ("none", 0)
  }

  def rowEncode(array: Array[Array[Int]]) = {
    val microcode = array.map(_.mkString(" ")).distinct // 通过构造一一映射利用distinct,这里使用mkString
    val addr = array.map { row => microcode.indexOf(row.mkString(" ")) }
    (microcode, addr)
  }

  def reuseRate(array: Array[Array[Int]]) = {
    (0 until array.size).map(i => array.take(i + 1).flatten).map(sub => sub.size / sub.distinct.size.toDouble)
  }

  def validVirtualAddr(array: Array[Array[Index]]) = assert(array.forall(row => row.map(_.coords(0)).distinct.size == row.size), "port conflict")


  def getGroups(array: matrix) = { // todo 类型拓展到T

    // 遍历得到冲突列表
    val conflict: mutable.Map[Int, Set[Int]] = Map[Int, Set[Int]]()
    array.foreach { row =>
      row.foreach { ele =>
        if (!conflict.contains(ele)) conflict += (ele -> Set[Int]())
        row.filterNot(_ == ele).foreach(conflict(ele) += _)
      }
    }
    //    println(
    //      conflict.map { case (index, forbidden) =>
    //        "index: " + index.toString + " forbidden: " + forbidden.mkString(" ")
    //      }
    //        .mkString("\n"))
    //    println
    // 依照冲突列表分组
    val groups = ArrayBuffer[Tuple2[Set[Int], Set[Int]]]()
    array.flatten.foreach { ele =>
      val setEle = Set(ele)
      if (groups.isEmpty) groups.append((setEle, conflict(ele)))
      else {
        val validGroups = groups.dropWhile(group =>
          !((group._1.intersect(conflict(ele)).isEmpty) && (group._2.intersect(setEle).isEmpty)))
        if (validGroups.isEmpty) {
          groups.append((setEle, conflict(ele)))
        } else {
          val groupToInsert = validGroups(0)
          groupToInsert._1 ++= setEle
          groupToInsert._2 ++= conflict(ele)
        }
      }
      //      println(
      //        groups.map { case (member, forbidden) =>
      //          "member: " + member.mkString(" ") + " forbidden: " + forbidden.mkString(" ")
      //        }
      //          .mkString("\n"))
      //      println
    }
    groups.map(_._1)
  }

  def getMap(grouped: mutable.Seq[Set[Int]]) = {
    val result = Map[Int, Tuple2[Int, Int]]()
    grouped.zipWithIndex.foreach { case (port, i) =>
      port.toSeq.zipWithIndex.foreach { case (elem, j) =>
        result += (elem -> (i, j))
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val a = Array(Array(1, 2, 3, 4, 5), Array(6, 7, 8, 9, 10), Array(11, 12, 13, 14, 15))
    val b = Array(Array(1, 3, 5), Array(2, 4, 6), Array(7, 9, 11), Array(8, 10, 12), Array(13, 14, 15))
    getGroups(a ++ b).foreach(members => println(members.mkString(" ")))

    val addrMap = getMap(getGroups(a ++ b))

    println(addrMap.mkString("\n"))

    printArray2D(a.map(_.map(addrMap)))
    printArray2D(b.map(_.map(addrMap)))

    printArray2D(MemAccessMap(a.map(_.map(addrMap))).addrSeqs)
    printArray2D(MemAccessMap(b.map(_.map(addrMap))).addrSeqs)
  }
}

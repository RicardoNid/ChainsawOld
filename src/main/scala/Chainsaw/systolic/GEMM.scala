package Chainsaw.systolic

import scala.collection.mutable.ArrayBuffer
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

object GEMM {

  def MM(A: ArrayBuffer[ArrayBuffer[Int]], B: ArrayBuffer[ArrayBuffer[Int]]) = {
    require(A(0).size == B.size)
    val N1 = A.size
    val N2 = B(0).size
    val N3 = A(0).size

    val C = ArrayBuffer.fill(N1, N2)(0)

    (0 until N1).foreach { i =>
      (0 until N2).foreach { j =>
        (0 until N3).foreach { k =>
          C(i)(j) = C(i)(j) + A(i)(k) * B(k)(j)
        }
      }
    }
    C
  }

  def MMAF(A: ArrayBuffer[ArrayBuffer[Int]], B: ArrayBuffer[ArrayBuffer[Int]], mode:String = "original") = {
    require(A(0).size == B.size)
    val N1 = A.size
    val N2 = B(0).size
    val N3 = A(0).size

    val theA, theB, theC = ArrayBuffer.fill(N1+1, N2+1, N3 + 1)(DSPRand.nextInt(100))

    def work(event: (Int, Int, Int) => Unit): Unit = {
      (1 to N1).foreach { i =>
        (1 to N2).foreach { j =>
          (1 to N3).foreach { k =>
            event(i, j, k)
          }
        }
      }
    }

    for (i <- 1 to N1; k <- 1 to N3) theA(i)(0)(k) = A(i - 1)(k - 1)
    for (j <- 1 to N2; k <- 1 to N3) theB(0)(j)(k) = B(k - 1)(j - 1)
    for (i <- 1 to N1; j <- 1 to N2) theC(i)(j)(0) = 0

    work { (i, j, k) =>
      // forwarding
      theA(i)(j)(k) = theA(i)(j - 1)(k)
      theB(i)(j)(k) = theB(i - 1)(j)(k)
      // calculation
      theC(i)(j)(k) = theC(i)(j)(k - 1) + theA(i)(j - 1)(k) * theB(i - 1)(j)(k)
    }
    // output
    (1 to N3).foreach{ k=>
      val CPage = theC.map(_.map(_.apply(k)))
      println(s"CPage$k: \n${CPage.map(_.mkString(" ")).mkString("\n")}")
      println()
    }

    theC.map(_.map(_.apply(N3)))
  }

  def MMAFHEXA(A: ArrayBuffer[ArrayBuffer[Int]], B: ArrayBuffer[ArrayBuffer[Int]], mode:String = "original") = {
    require(A(0).size == B.size)
    val N1 = A.size
    val N2 = B(0).size
    val N3 = A(0).size

    val theA, theB, theC = ArrayBuffer.fill(N1+1, N2+1, N3 + 1)(DSPRand.nextInt(100))

    def workByHEXA(event: (Int, Int, Int) => Unit): Unit = {
      (1 to N1).foreach { i =>
        (1 to N2).foreach { j =>
          (1 to N3).foreach { k =>

            event(i, j, k)
          }
        }
      }
    }

    // input
    for (i <- 1 to N1; k <- 1 to N3) theA(i)(0)(k) = A(i - 1)(k - 1)
    for (j <- 1 to N2; k <- 1 to N3) theB(0)(j)(k) = B(k - 1)(j - 1)
    for (i <- 1 to N1; j <- 1 to N2) theC(i)(j)(0) = 0

    // "work"
    workByHEXA { (i, j, k) =>
      // forwarding
      theA(i)(j)(k) = theA(i)(j - 1)(k)
      theB(i)(j)(k) = theB(i - 1)(j)(k)
      // calculation
      theC(i)(j)(k) = theC(i)(j)(k - 1) + theA(i)(j - 1)(k) * theB(i - 1)(j)(k)
    }

    // output
    (1 to N3).foreach{ k=>
      val CPage = theC.map(_.map(_.apply(k)))
      println(s"CPage$k: \n${CPage.map(_.mkString(" ")).mkString("\n")}")
      println()
    }

    theC.map(_.map(_.apply(N3)))
  }


  def main(args: Array[String]): Unit = {
    val A = ArrayBuffer.tabulate(3, 4)(_ + _)
    val B = ArrayBuffer.tabulate(4, 5)(_ + _)

    println(A.map(_.mkString(" ")).mkString("\n"))
    println()
    println(B.map(_.mkString(" ")).mkString("\n"))
    println()
    println(MM(A, B).map(_.mkString(" ")).mkString("\n"))
    println()
    //    println(MMAF(A, B).map(_.mkString(" ")).mkString("\n"))
    println(MMAF(A, B).map(_.mkString(" ")).mkString("\n"))
  }
}


package Chainsaw.examples

import Chainsaw.printlnGreen
import spinal.core.internals.{BaseNode, Expression}
import spinal.core.{BaseType, Component, GlobalData, Mem, MemReadAsync, MemReadSync, MemReadWrite, MemWrite, SpinalError}

import scala.collection.mutable.ArrayBuffer

object MyLatencyAnalysis {

  def apply(paths: Expression*): Int = list(paths)

  def list(paths: Seq[Expression]): Int = paths.init.zip(paths.tail).map { case (e0, e1) => findShortest(e0, e1) }.sum

  def findShortest(from: Expression, to: Expression): Int = {

    var depth = 0

    // we need a "mark" for walked node, we generate one by allocateAlgoIncrementale
    // the specific value of walkedId has no effect
    val walkedId = GlobalData.get.allocateAlgoIncrementale()

    val currentQueue, prevQueue = new ArrayBuffer[BaseNode]

    def showQueues() = {
      println(
        Seq(currentQueue, prevQueue)
          .zip(Seq("current", "prev   "))
          .map { case (nodes, i) =>
            s"$i: ${nodes
              .map {
                case baseType: BaseType => baseType.toString()
                case mem: Mem[_]        => mem.toString()
              }
              .mkString(" ")}"
          }
          .mkString("\n")
      )
      print(s"walked: ")
      Component.current.foreachReflectableNameables {
        case node: BaseType => if (node.algoIncrementale == walkedId) print(s"${node.getName()} ")
        case node: Mem[_]   => if (node.algoIncrementale == walkedId) print(s"${node.getName()} ")
        case _              =>
      }
      println()
    }

    currentQueue += to
    printlnGreen("init")
    showQueues()
    while (currentQueue.nonEmpty) { // while the queues are not all empty

      // do traceback for nodes in queue 0
      currentQueue.foreach(node =>
        if (traceBack(node, "queue")) {
          printlnGreen("final status");
          showQueues();
          return depth
        }
      )
      printlnGreen("after tracingBack")
      showQueues()

      currentQueue.clear()
      currentQueue ++= prevQueue
      prevQueue.clear()

      depth += 1
    }

    // traceBack from the destination node, keeping tracing until the "distance" is 1
    def traceBack(des: BaseNode, desType: String = "not specified"): Boolean = {

      // conditions when we skip the traceBack
      if (des.algoIncrementale == walkedId) return false // already walked, skip
      if (des == from) return true // get the source, succeed

      // do traceBack, the basic ideas are
      //  when delay = 0, recursively tracing back drivers' drivers
      //  when delay = 1, stop recursion and add the drivers to prevQueue
      des.algoIncrementale = walkedId // mark the des as walked
      val succeed = des match {
        case that: Mem[_] => { // Mem, delay write -> mem = 1, thus, add all write ports to prevQueue
          that.foreachStatements {
            case port: MemWrite     => port.foreachDrivingExpression(input => prevQueue += input)
            case port: MemReadWrite => port.foreachDrivingExpression(input => prevQueue += input)
            case _                  => // do nothing
          }
          false
        }
        case that: BaseType => { // BaseType, for reg, delay = 1, for others, delay = 0
          def walkInputs(func: BaseNode => Unit) = {
            that.foreachStatements(s => {
              s.foreachDrivingExpression(func)
              s.walkParentTreeStatementsUntilRootScope(func)
            })
          }
          // delay = 1
          if (that.isReg) walkInputs(input => prevQueue += input)
          else walkInputs(input            => if (traceBack(input, "basetype")) return true)
          false
        }
        // following are three kind of read ports, mem of these ports need to be handled independently, as the won't be visited by foreachDrivingExpression
        case that: MemReadSync => {
          // mem -> read delay = 1, so we add the mem to the prev queue
          // at the same time, for other drivers, delay = 0
          that.foreachDrivingExpression(input => if (traceBack(input, "readsync")) return true)
          prevQueue += that.mem
          false
        }
        case that: MemReadWrite => { // when looking from the des side, it's the same as readSync
          that.foreachDrivingExpression(input => if (traceBack(input, "readwritesync")) return true)
          prevQueue += that.mem
          false
        }
        case that: MemReadAsync => { // readAsync, delay = 0
          that.foreachDrivingExpression(input => if (traceBack(input, "readasync")) return true)
          if (traceBack(that.mem, "mem")) return true
          false
        }
        case that: Expression => { // other expressions, no delay, so we invoke traceBack recursively
          that.foreachDrivingExpression(input => if (traceBack(input, "others")) return true)
          false
        }
      }
      //      printlnGreen(s"type = $desType, des = $des")
      //      showQueues()
      succeed
    }

    SpinalError("latencyAnalysis don't find any path")
    -1
  }
}

package Chainsaw.MCM

import Chainsaw.MCM.RAGn._

import scala.collection.mutable.ListBuffer

object MAG {
  def apply(): Unit = {
    //  step 2, building cost-0
    costLUT += 1 -> 0
    fundamentalsLUT += 1 -> ListBuffer(ListBuffer(1))
    postProcess

    //  step 3, building cost-1
    ASetOnSets(costNcoeffs(0), costNcoeffs(0), range, asymmetric = false).foreach { coeff =>
      if (!costLUT.contains(coeff)) {
        costLUT += coeff -> 1
        fundamentalsLUT += coeff -> ListBuffer(ListBuffer(1, coeff))
      }
    }
    postProcess

    //  step 4, building cost-2
    costNcoeffs(1).foreach { coeff => //  start from cost-1 implementations
      ASetOnSets(ListBuffer(coeff), lookupPaths(coeff).head, range) //  A(cost-1, cost-1-fundamental)
        .foreach { newCoeff => //  for a newly found coefficient
          val newPath = ListBuffer(1, coeff, newCoeff) //  the new path should be
          addToLUT(newCoeff, 2, newPath)
        }
    }
    postProcess

    //  step 5, building cost-3
    costNcoeffs(2) //  patern 1 ~ 6, starts from cost-2 implementation
      .foreach { case coeff => //  for each cost-2 implementation
        lookupPaths(coeff).foreach { path => //  for each specific path
          ASetOnSets(path, ListBuffer(coeff), range) //  A(cost-2, cost-2-fundamental)
            .foreach { newCoeff => //  strategy below is the same as step 4
              val newPath = path ++ ListBuffer(newCoeff)
              addToLUT(newCoeff, 3, newPath)
            }
        }
      }

    //  patern 7, cost-1 + cost-1
    for (coeff0 <- costNcoeffs(1); coeff1 <- costNcoeffs(1)) {
      if (coeff0 >= coeff1) { //  upper triangle, avoid same combination
        ASet(coeff0, coeff1, range)
          .foreach { newCoeff =>
            val newPath = ListBuffer(1, coeff0, coeff1, newCoeff)
            addToLUT(newCoeff, 3, newPath)
          }
      }
    }
    postProcess

    //  step 6 building cost-4
    costNcoeffs(3) //  patern 1 ~ 27, starts from cost-3 implementation
      .foreach { case coeff =>
        lookupPaths(coeff).foreach { path => //  for a specific path
          ASetOnSets(path, ListBuffer(coeff), range) //  A(cost-3, cost-3-fundamental)
            .foreach { newCoeff => //  strategy below is the same as step 4
              val newPath = path ++ ListBuffer(newCoeff)
              addToLUT(newCoeff, 4, newPath)
            }
        }
      }
    postProcess

    //  patern 28,29,32, cost-2 + cost-1
    for (coeff0 <- costNcoeffs(1); coeff1 <- costNcoeffs(2)) {
      ASet(coeff0, coeff1, range).foreach { newCoeff =>
        val newPath = ListBuffer(1, coeff0, coeff1, newCoeff)
        addToLUT(newCoeff, 4, newPath)
      }
    }
    postProcess

    // TODO: implement pattern 30, 31
    costNcoeffs(2) //  pattern 30
      .foreach { coeff =>
        lookupPaths(coeff).foreach { path =>
          val nodesBelow = ASet(path(0), path(1), range)
          nodesBelow.foreach { nodeBelow =>
            ASet(nodeBelow, coeff, range).foreach { newCoeff =>
              val newPath = ListBuffer(path(0), path(1), coeff, nodeBelow, newCoeff)
              addToLUT(newCoeff, 4, newPath)
            }
          }
        }
      }
    postProcess

    costNcoeffs(2) //  pattern 31
      .foreach { coeff =>
        lookupPaths(coeff).foreach { path =>
          val nodesBelow = ASet(path(1), path(1), range)
          nodesBelow.foreach { nodeBelow =>
            ASet(nodeBelow, coeff, range).foreach { newCoeff =>
              val newPath = ListBuffer(path(0), path(1), coeff, nodeBelow, newCoeff)
              addToLUT(newCoeff, 4, newPath)
            }
          }
        }
      }
    postProcess

    println(costNcoeffs(4).map(lookupPaths(_)).head.map(_.mkString("->")).mkString("\n"))
  }
}

import scala.collection.mutable.ListBuffer

def getHyperbolicSequence(iteration: Int) = {
  require(iteration < 54, "iteration times should be less than 54")
  val sequence = (0 to 4) ++ (4 to 13) ++ (13 to 40) ++ (40 to 50)
  sequence.slice(0, iteration)
}

getHyperbolicSequence(15)

def getHyperbolicSequence(iterations: Int) = { // length = iterations

  require(iterations >= 1)

  var currentSpecial = 1
  var i = iterations - 1
  val result = ListBuffer(1)
  while (i > 0) {
    if (result.last == (currentSpecial * 3 + 1)) {
      result += result.last
      currentSpecial = currentSpecial * 3 + 1
    }
    else result += (result.last + 1)
    i -= 1
  }
  result.toIndexedSeq
}

getHyperbolicSequence(15)
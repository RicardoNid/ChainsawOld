var temp = 0

def getTemp = temp

def printGetTemp0(tempGetter: => Int) = {
  println(tempGetter)
  temp = 1
  println(tempGetter)
}

def printGetTemp1(tempGetter: Int) = {
  println(tempGetter)
  temp = 1
  println(tempGetter)
}

temp = 0
printGetTemp0(getTemp)

temp = 0
printGetTemp1(getTemp)






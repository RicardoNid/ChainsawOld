import scala.io.Source
//val patternShiftAdd = "([P|M]?[0-9]*X).*([P|M]?[0-9]*X)<<([0-9]+).*([P|M]?[0-9]*X)".r
//val line1 = "P31X <-  X<<5  + M1X"
//val line2 = ""
//val patternShiftAdd(sum, left, shift, right) = line1
//val patternShiftAdd(sum, left, shift, right) = line2

val patternLUT = "LUT=([0-9]*)".r.unanchored

val records = Source.fromFile("/home/ltr/IdeaProjects/Chainsaw/AreaReport.txt").getLines().take(225).toSeq
def getLUT(string: String) = {
  val patternLUT(digits) = string
  digits.toInt
}

val mine0 = (0 until 225).filter(_ % 3 == 0).map(i => records(i)).map(getLUT).sum
val mine1 = (0 until 225).filter(_ % 3 == 1).map(i => records(i)).map(getLUT).sum
val theirs = (0 until 225).filter(_ % 3 == 2).map(i => records(i)).map(getLUT).sum
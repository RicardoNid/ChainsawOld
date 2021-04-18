import Margin.Margin

object Margin extends Enumeration {
  type Margin = Value
  val TOP, BOTTOM, LEFT, RIGHT = Value
}

def matchMargin(margin: Margin) = {
  margin match {
    case Margin.TOP =>
    case Margin.BOTTOM =>
    case Margin.LEFT =>
    case Margin.RIGHT =>
  }
}
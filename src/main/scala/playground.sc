import scala.util.Random

def randomTimeSequence(length: Int): String = {
  val r = new Random()

  def op(code: Int) = {
    code match {
      case 0 => "+"
      case 1 => "-"
      case 2 => "*"
      case 3 => "/"
      case 4 => "%"
    }
  }

  "t" + (0 until length).map(_ => op(r.nextInt(5)) + r.nextInt(100).toString).mkString("")
}

(0 until 100).foreach(_ => println(randomTimeSequence(20)))

println((0 until 100).map(_ * 11 / 49).mkString(" "))
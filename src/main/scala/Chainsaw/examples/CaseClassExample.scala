package Chainsaw.examples

object CaseClassExample extends App {

  case class Pair(a: Int, b: Int)

  val temp0 = Pair(0, 1)
  val temp1 = Pair(0, 1)
  println(temp0.hashCode())
  println(temp1.hashCode())

  class FormalPair(a: Int, b: Int)

  val temp2 = new FormalPair(0, 1)
  val temp3 = new FormalPair(0, 1)
  println(temp2.hashCode())
  println(temp3.hashCode())

  class Package(val orgName:String, val packageName:String, val packageVersion:String){
    override def toString: String = s"$orgName-$packageName-$packageVersion"

    override def hashCode(): Int =  toString.hashCode

    override def equals(obj: Any): Boolean = obj.isInstanceOf[Package] && toString.equals(obj.toString)
  }
}

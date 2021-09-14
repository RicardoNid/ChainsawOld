
import scala.reflect.runtime.universe._
def matchList[A: TypeTag](list: List[A]) = list match {
  case strlist: List[String @unchecked] if typeOf[A] =:= typeOf[String] => println("A list of strings!")
  case intlist: List[Int @unchecked] if typeOf[A] =:= typeOf[Int] => println("A list of ints!")
}

matchList(List(1))
matchList(List("news"))


import breeze.numerics.pow

def verify(res:String, num:Int): Boolean ={
  (0 until res.length).map { i =>
    res(i) match {
      case '0' => 0
      case '1' => 1 * pow(2, i)
      case '9' => -1 * pow(2, i)
    }
  }.sum == num
}

def classicCSD(num: Int):String = {
  val pattern = "11+0".r

  var string = num.toBinaryString.reverse + "0"
  var done = false
  while(!done){
    val sub: Option[String] = pattern.findFirstIn(string)
    sub match {
      case Some(x) => string = string.replaceFirst(x, "9" + "0" * (x.length - 2) + "1")
      case None => done = true
    }
  }

  assert(verify(string, num))
  print("success")
  string.reverse
}

def optimalCSD(num: Int):String = {
  val pattern0 = "1101".r
  val pattern1 = "11+0".r
  val pattern2 = "901".r

  var string = num.toBinaryString.reverse + "0"
  var done0 = false
  var done1 = false

  while(!done0){
    val sub = pattern0.findFirstIn(string).getOrElse(
      pattern1.findFirstIn(string).getOrElse {
        done0 = true
        " "
      }
    )
    if (sub == "1101") string = string.replaceFirst(sub, "9011")
    else string = string.replace(sub, "9" + "0" * (sub.length - 2) + "1")
  }

  while(!done1){
    val sub = pattern2.findFirstIn(string).getOrElse{
      done1 = true
      " "
    }
    string = string.replaceFirst(sub, "110")
  }

  assert(verify(string, num))
  println(s"$num is encoded as ${string.reverse}")
  string.reverse
}

for (elem <- (0 until 100)) {optimalCSD(elem)}

import spinal.core._
import spinal.lib._
import spinal.core.sim._

log2Up(1)



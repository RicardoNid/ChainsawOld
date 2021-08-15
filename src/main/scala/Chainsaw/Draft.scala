package Chainsaw

import com.mathworks.engine._
import com.mathworks.matlab.types.Complex
import com.mathworks.matlab.{types => MTypes}
import Chainsaw._

import java.lang.Thread.sleep
import java.time.Instant
import java.time.Duration


object Draft extends App {
  println(BigInt(35).toWords(4).mkString(" "))
  println(BigInt(35).showWords(4).mkString(" "))
  println(BigInt(35).showWordsHex(8).mkString(" "))

  println(BigInt(35) ^ BigInt(23))
}
package Chainsaw.tobeTransplanted

import Chainsaw._
import spinal.core._

import scala.reflect.runtime.{universe => ru}
import scala.sys.process.Process

abstract class FloPoCoBlackBox[inputType <: Data, outputType <: Data] extends BlackBox {
  val input: inputType
  val output: outputType

  val operatorName: String

  // helper functions for reflection
  /** Just copy the line below, this is a must-be
    *
    * override def ruType = ru.typeOf[this.type]
    */
  def ruType: ru.Type // TODO: must be implemented in concrete class, why?

  // TODO: add this usage of reflection to my cookbook
  def getArgValueString(argName: String) = {
    val m          = ru.runtimeMirror(this.getClass.getClassLoader)
    val im         = m.reflect(this)
    val termSymb   = ruType.decl(ru.TermName(argName)).asTerm
    val termMirror = im.reflectField(termSymb)
    termMirror.get.toString
  }

  // determine names ant paths by reflection
  val fileds          = this.getClass.getDeclaredFields.map(_.getName)
  val argNumber       = this.getClass.getConstructors.head.getParameters.length
  val argNames        = fileds.take(argNumber)
  val argValues       = argNames.map(getArgValueString)
  def blackBoxName    = (operatorName +: argValues).reduce(_ + "_" + _) + "_F400_uid2"
  def rtlPath         = defaultOutputDir + s"/$blackBoxName.vhdl"
  def operatorCommand = operatorName + " " + argNames.zip(argValues).map { case (name, value) => name + "=" + value }.mkString(" ")

  def invokeFloPoCo() = {
    setBlackBoxName(blackBoxName)
    addRTLPath(rtlPath)
    printlnYellow(s"invoke flopoco: $operatorCommand")
    Process(s"./flopoco outputFile=$rtlPath $operatorCommand", new java.io.File(flopocoPath)) !
  }
}

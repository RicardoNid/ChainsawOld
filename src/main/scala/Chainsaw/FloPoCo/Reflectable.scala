package Chainsaw.FloPoCo

import scala.reflect.runtime.{universe => ru}

trait Reflectable {
  def getArgValueString(argName: String) = {
    println(this.getClass)
    println(argName)
    val m = ru.runtimeMirror(this.getClass.getClassLoader)
    val im = m.reflect(this)
    val termSymb = ru.typeOf[this.type].decl(ru.TermName(argName)).asTerm
    val termMirror = im.reflectField(termSymb)
    termMirror.get.toString
  }
}

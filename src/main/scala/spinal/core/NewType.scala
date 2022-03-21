package spinal.core

// template of implementing new types
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object NewTypeFactory extends TypeFactory {
  def NewType = new NewType
}

//trait BaseTypeFactory extends BoolFactory with BitsFactory with UIntFactory with SIntFactory with VecFactory
//  with SFixFactory with UFixFactory with SRealFactory with URealFactory with RealFactory with NewTypeFactory

class NewType extends MultiData {

  val raw = SInt(4 bits)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer("" -> raw)
  override private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    this.raw := that.asInstanceOf[NewType].raw
  }
}

class UsingNewType extends Component {

  import NewTypeFactory._

  val input  = in(NewType)
  val output = out(NewType)

  output := input
}
object UsingNewType {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new UsingNewType)
  }
}

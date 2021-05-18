case class Purchase(name: String, orderNumber: Int, var shipped: Boolean)
val p = Purchase("Jeff Lebowski", 23819, false)
import scala.reflect.runtime.{universe => ru}
val m = ru.runtimeMirror(p.getClass.getClassLoader)
val shippingTermSymb = ru.typeOf[Purchase].decl(ru.TermName("shipped")).asTerm
val im = m.reflect(p)
val shippingFieldMirror = im.reflectField(shippingTermSymb)
shippingFieldMirror.get
package Chainsaw

import java.util.function.Supplier

package object MCM {

  val intSupplier = new Supplier[Int] {
    private var id = -1
    override def get(): Int = {
      id += 1; id
    }
  }

}

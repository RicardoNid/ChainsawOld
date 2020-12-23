package sysu.xilinx

trait Strategy

object FlattenHierarchy extends Enumeration {
  type FlattenHierarchy = Value
  val none, rebuilt, full = Value

  def optionName = "flatten_hierarchy"

  def strategyName(choice: FlattenHierarchy): String = {
    choice match {
      case `none` => "none"
      case `rebuilt` => "rebuilt"
      case `full` => "full"
    }
  }
}

import FlattenHierarchy._

case class SynthStrategy(
                          flatten_hierarchy: FlattenHierarchy = full
                        ) extends Strategy

package Chainsaw

import spinal.core.BaseType
import spinal.core.internals.{Expression, Statement}

object ComputationExtrction {
  def apply(des: Expression): Unit = {

    des match {
      case des: BaseType => des.foreachStatements { s: Statement =>
        s.foreachDrivingExpression { d: Expression =>
          d match {
            case d: BaseType => {
              println(s"is reg: ${d.isReg}\nop name: ${d.opName}")
              d.foreachStatements { s1: Statement =>
                s1.foreachDrivingExpression { d1: Expression =>
                  println(s"op name: ${d1.opName}")
                }
              }
            }
          }
        }
      }
    }
  }
}


package Chainsaw.dataflow

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

/** build a flow converter for a flow conversion according to a register allocation
 */
case class FlowConverter[T <: Data](conversion: FlowConversion, registerAllocation: RegisterAllocation, hardType: HardType[T])
  extends Component {

  val dataIn = slave Flow Fragment(Vec(hardType(), conversion.flowIn.width))
  val dataOut = master Flow Fragment(Vec(hardType(), conversion.flowOut.width))

  val counter = CounterFreeRun(conversion.period)
  when(dataIn.last)(counter.clear())

  // basic connection
  val registers = Seq.fill(registerAllocation.registerCount)(Reg(hardType()))
  registers.head.assignDontCare()
  registers.sliding(2).foreach { pair => pair(1) := pair(0) }
  dataOut.payload.foreach(_.assignDontCare())

  def getTimeIn(data: Int) = conversion.flowIn.getTime(data)

  def getTimeOut(data: Int) = conversion.flowOut.getTime(data) + conversion.latency

  def getPortIn(data: Int) = conversion.flowIn.getPort(data)

  def getPortOut(data: Int) = conversion.flowOut.getPort(data)

  // input connections
  (0 until conversion.dataCount).foreach { data =>
    val time = getTimeIn(data)
    val registerIndex = registerAllocation.occupation(time + 1).indexOf(data)
    if(registerIndex == -1) when(counter.value === time)(dataOut.payload(getPortOut(data)) := dataIn.payload(getPortIn(data)))
    else when(counter.value === time)(registers(registerIndex) := dataIn.payload(getPortIn(data)))
  }

  // output connections
  (0 until conversion.dataCount).foreach { data =>
    val time = getTimeOut(data)
    val registerIndex = registerAllocation.occupation(time).indexOf(data)
    if(registerIndex != -1) when(counter.value === time)(dataOut.payload(getPortOut(data)) := registers(registerIndex))
  }

  // TODO: inner connections(for forward allocation, this is not necessary)

  // controls
  switch(counter.value) {
    conversion.flowOut.validCycles.foreach(time => is(time)(dataOut.valid := True))
    default(dataOut.valid := False)
  }
  dataOut.last := Delay(dataIn.last, conversion.latency, init = False)
}
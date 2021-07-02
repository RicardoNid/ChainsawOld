package Chainsaw.Memories

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class XILINX_BRAM_PORT(dataWidth: Int, addressWidth: Int) extends Bundle with IMasterSlave {
  val dataIn = Bits(dataWidth bits)
  val addr = UInt(addressWidth bits)
  val dataOut = Bits(dataWidth bits)
  val en = Bool
  val we = Bool

  override def asMaster(): Unit = {
    in(dataOut)
    out(dataIn, addr, we, en)
  }

  def >>(readPort: XILINX_BRAM_PORT_READ): Unit = {
    addr := readPort.addr
    en := readPort.en
    we := False
    dataIn := dataIn.getZero // TODO: don't care
    readPort.dataOut := dataOut
  }

  def <<(writePort: XILINX_BRAM_PORT_WRITE): Unit = {
    addr := writePort.addr
    en := writePort.en
    we := writePort.we
    dataIn := writePort.dataIn
  }

  def doRead(addrIn: UInt) = {
    addr := addrIn
    en := True
    we := False
  }

  def doWrite(addrIn: UInt, data: Bits) = {
    addr := addrIn
    en := True
    we := True
    dataIn := data
  }

  def simRead(addrIn: BigInt) = {
    addr #= addrIn
    en #= true
    we #= false
  }

  def simWrite(addrIn: BigInt, data: BigInt) = {
    addr #= addrIn
    en #= true
    we #= true
    dataIn #= data
  }

  def preAssign(): Unit = {
    addr.clearAll()
    addr.allowOverride
    en.clear()
    en.allowOverride
    we.clear()
    we.allowOverride
    dataIn.clearAll()
    dataIn.allowOverride
  }
}

case class XILINX_BRAM_PORT_READ(dataWidth: Int, addressWidth: Int) extends Bundle with IMasterSlave {
  val addr = UInt(addressWidth bits)
  val dataOut = Bits(dataWidth bits)
  val en = Bool
  override def asMaster(): Unit = {
    in(dataOut)
    out(addr, en)
  }
}

case class XILINX_BRAM_PORT_WRITE(dataWidth: Int, addressWidth: Int) extends Bundle with IMasterSlave {
  val addr = UInt(addressWidth bits)
  val dataIn = Bits(dataWidth bits)
  val en = Bool
  val we = Bool
  override def asMaster(): Unit = {
    out(dataIn, addr, en, we)
  }
}

case class XILINX_BRAM18E2() extends Component {

  val dataWidth = 2 * (8 + 1) // core width = 36
  val addressWidth = 10

  val ioA = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val ioB = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  val bram = Mem(Bits(dataWidth bits), BigInt(1) << addressWidth)

  ioA.dataOut := bram.readWriteSync(ioA.addr, ioA.dataIn, ioA.en, ioA.we)
  ioB.dataOut := bram.readWriteSync(ioB.addr, ioB.dataIn, ioB.en, ioB.we)
}

case class XILINX_BRAM36E2() extends Component {

  val dataWidth = 4 * (8 + 1) // core width = 36
  val addressWidth = 10

  val ioA = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val ioB = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  val bram = Mem(Bits(dataWidth bits), BigInt(1) << addressWidth)

  ioA.dataOut := bram.readWriteSync(ioA.addr, ioA.dataIn, ioA.en, ioA.we)
  ioB.dataOut := bram.readWriteSync(ioB.addr, ioB.dataIn, ioB.en, ioB.we)
}

case class mWnRRAM(m: Int, n: Int) extends Component {

  //  // replication
  //  val io = new Bundle {
  //    val reads = Seq.fill(n)(slave(XILINX_BRAM_PORT_READ(18, 10)))
  //    val writes = Seq.fill(m)(slave(XILINX_BRAM_PORT_WRITE(18, 10)))
  //  }
  //
  //  // replication
  //  require(m == 1)
  //  val rams = Seq.fill(n)(XILINX_BRAM18E2())
  //  rams.zip(io.reads).foreach { case (ram, port) => ram.ioA >> port }
  //  rams.zip(io.writes).foreach { case (ram, port) => ram.ioA << port }

  // multipumping
  val io = new Bundle {
    val reads = Seq.fill(n)(slave(XILINX_BRAM_PORT_READ(18, 10)))
    val writes = Seq.fill(m)(slave(XILINX_BRAM_PORT_WRITE(18, 10)))
  }
  io.reads.foreach(_.dataOut.clearAll())

  // multipumping
  val ram = XILINX_BRAM18E2()
  ram.ioA.preAssign()
  ram.ioB.preAssign()

  val writeAddr = io.writes.map(port => RegNext(port.addr))
  val writeData = io.writes.head.dataIn +: io.writes.tail.map(port => RegNext(port.dataIn))
  val readAddr = io.reads.head.addr +: io.reads.tail.map(port => RegNext(port.addr))
  val readData = io.reads.init.map(port => Reg(port.dataOut)) :+ io.reads.last.dataOut
  Seq(writeAddr, writeData, readAddr, readData).foreach(_.foreach(_.addTag(crossClockDomain)))

  io.reads.foreach(_.dataOut.allowOverride)
  io.reads.init.zip(readData.init).foreach { case (port, reg) => port.dataOut := reg }

  val multiplexCounter = CounterFreeRun(m + n)
  switch(multiplexCounter.value) {

    (0 until n).foreach { readIndex =>
      is(U(readIndex)) {
        ram.ioA.addr := readAddr(readIndex)
        ram.ioA.en := True
        ram.ioA.we := False
        if (readIndex != 0) readData(readIndex - 1) := ram.ioA.dataOut
      }
    }
    (0 until m).foreach { writeIndex =>
      is(U(writeIndex + n)) {
        ram.ioA.doWrite(writeAddr(writeIndex), writeData(writeIndex))
        if (writeIndex == 0) readData(n - 1) := ram.ioA.dataOut
      }
    }
  }
}

case class MemoryArea() extends Component {
  val globalClockDomain = ClockDomain.external("global",
    config = ClockDomainConfig(resetKind = BOOT))

  val globalClokingArea = new ClockingArea(globalClockDomain) {
    val ramClockDomain = ClockDomain.external("ramClockDomain",
      config = ClockDomainConfig(
        resetKind = BOOT, resetActiveLevel = HIGH)
    )

    // default clocking area
    val io = new Bundle {
      val writes = Seq.fill(1)(slave(XILINX_BRAM_PORT_WRITE(18, 10)))
      val reads = Seq.fill(3)(slave(XILINX_BRAM_PORT_READ(18, 10)))
      val forClock = out Bool
    }

    // ram clocking area
    val ramClockingArea = new ClockingArea(ramClockDomain) {
      val mem = mWnRRAM(1, 3)
      mem.ram.bram.simPublic()
    }
    io.writes.zip(ramClockingArea.mem.io.writes).foreach { case (outer, inner) => outer <> inner }
    io.reads.zip(ramClockingArea.mem.io.reads).foreach { case (outer, inner) => outer <> inner }

    io.forClock := RegNext(io.reads(0).dataOut.lsb)
  }
}

object XILINX_BRAM18E2 {
  def main(args: Array[String]): Unit = {
    GenRTL(new MemoryArea())
    //    VivadoSynth(new MemoryArea)
    //    VivadoImpl(new mWnRRAM(1, 3))

    val writeNum = 1
    val readNum = 3
    def mWnRSim() = {
      SimConfig.withWave.compile(new MemoryArea()).doSimUntilVoid { dut =>
        import dut._
        globalClockDomain.forkStimulus(8)
        import dut.globalClokingArea._
        ramClockDomain.forkStimulus(2)

        val outerMonitor = fork {
          while (true){
            globalClockDomain.waitSampling()
            println(io.reads.map(_.dataOut.toBigInt).mkString(" "))
          }
        }

        io.writes.foreach { port =>
          port.we #= false
          port.en #= false
          port.dataIn #= 0
          port.addr #= 0
        }

        def write(port: Int, addr: BigInt, data: BigInt) = {
          io.writes(port).addr #= addr
          io.writes(port).dataIn #= data
          io.writes(port).en #= true
          io.writes(port).we #= true
        }

        def read(port: Int, addr: BigInt) = {
          val readPort = io.reads(port)
          readPort.addr #= addr
          readPort.en #= true
          readPort.dataOut.toInt
        }

        def read123() = {
          read(0, 1)
          read(1, 2)
          read(2, 3)
        }


        def doWR(addr: Int, data: Int) = {
          write(0, addr, data)
          read123()
          sleep(8)
        }

        doWR(1, 13)
        doWR(2, 17)
        doWR(3, 19)
        doWR(3, 19)
        doWR(3, 19)
        doWR(3, 19)

        simSuccess()
      }
    }
    def bramSim() = {
      SimConfig.withWave.compile(new XILINX_BRAM18E2).doSim { dut =>
        import dut._
        def doRead(portId: Int, addr: BigInt) = {
          val port = if (portId == 0) ioA else ioB
          port.addr #= addr
          port.en #= true
          port.we #= false
        }
        def doWrite(portId: Int, addr: BigInt, data: BigInt) = {
          val port = if (portId == 0) ioA else ioB
          port.addr #= addr
          port.dataIn #= data
          port.en #= true
          port.we #= true
        }

        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()
        doWrite(0, 0, 1)
        doWrite(1, 12, 5)
        clockDomain.waitSampling()
        doRead(0, 0)
        doRead(1, 12)
        clockDomain.waitSampling()
        doRead(0, 0)
        doRead(1, 12)
        clockDomain.waitSampling()
        doRead(0, 0)
        doRead(1, 12)
        sleep(20)
      }
    }

    mWnRSim()
  }
}
